"""Manage Paperless-ngx users, groups, and object-level permissions.

Loaded as a Django app via PAPERLESS_APPS. On startup it:

1. Creates an "editors" group (view/add/change/delete on all models)
   and assigns all remote users (containing "@") to it.
2. Creates an "opencrow" service account in a "viewers" group
   (view/add/change on documents + mail models, no delete).
3. Grants object-level view+change to opencrow on all existing objects.
4. Installs post_save signals so new objects and new remote users are
   handled automatically in every worker process.

Django model imports are deferred because the app registry is not yet
populated at module import time — top-level imports would raise
AppRegistryNotReady.
"""

import logging

from django.apps import AppConfig
from django.db.models.signals import post_save

logger = logging.getLogger(__name__)

OPENCROW_USERNAME = "opencrow"

# Object-level permissions granted to opencrow on each model
OPENCROW_PERM_MAP: dict[str, tuple[str, str]] = {
    "Document": ("view_document", "change_document"),
    "Tag": ("view_tag", "change_tag"),
    "Correspondent": ("view_correspondent", "change_correspondent"),
    "DocumentType": ("view_documenttype", "change_documenttype"),
    "StoragePath": ("view_storagepath", "change_storagepath"),
}

# Model-level permissions for the viewers group (opencrow)
VIEWERS_ALLOWED_APPS = {"documents", "paperless_mail"}
VIEWERS_ALLOWED_ACTIONS = {"view", "add", "change"}


def _grant_opencrow_object_perms(
    sender: type,
    instance: object,
    created: bool,
    **kwargs: object,
) -> None:
    """post_save handler: grant opencrow view+change on new objects."""
    if not created:
        return

    from django.contrib.auth.models import User
    from guardian.shortcuts import assign_perm

    perms = OPENCROW_PERM_MAP.get(sender.__name__)
    if perms is None:
        return

    try:
        user = User.objects.get(username=OPENCROW_USERNAME)
    except User.DoesNotExist:
        return

    for perm in perms:
        assign_perm(perm, user, instance)


def _add_remote_user_to_editors(
    sender: type,
    instance: object,
    created: bool,
    **kwargs: object,
) -> None:
    """post_save handler: add new remote users (with @) to editors group."""
    if not created:
        return

    from django.contrib.auth.models import Group

    username = getattr(instance, "username", "")
    if "@" not in username:
        return

    try:
        editors = Group.objects.get(name="editors")
    except Group.DoesNotExist:
        return

    if not instance.groups.filter(name="editors").exists():  # type: ignore[union-attr]
        instance.groups.add(editors)  # type: ignore[union-attr]
        logger.info("Auto-added remote user %s to editors group", username)


def _setup_editors_group() -> None:
    """Create editors group with full CRUD permissions for all models."""
    from django.contrib.auth.models import Group, Permission, User
    from django.contrib.contenttypes.models import ContentType

    editors_group, _ = Group.objects.get_or_create(name="editors")

    permissions = []
    for ct in ContentType.objects.all():
        permissions.extend(
            Permission.objects.filter(
                content_type=ct,
                codename__regex=r"^(view|change|add|delete)_",
            )
        )
    editors_group.permissions.set(permissions)

    for user in User.objects.filter(username__contains="@"):
        if not user.groups.filter(name="editors").exists():
            user.groups.add(editors_group)

    logger.info("editors group: %d permissions", len(permissions))


def _setup_opencrow_user() -> None:
    """Create opencrow user/viewers group and grant object-level permissions."""
    from django.contrib.auth.models import Group, Permission, User
    from django.contrib.contenttypes.models import ContentType
    from documents.models import (
        Correspondent,
        Document,
        DocumentType,
        StoragePath,
        Tag,
    )
    from guardian.shortcuts import assign_perm

    # Create viewers group with model-level perms (no delete)
    viewers_group, _ = Group.objects.get_or_create(name="viewers")
    viewer_perms = [
        p
        for ct in ContentType.objects.all()
        for p in Permission.objects.filter(content_type=ct)
        if ct.app_label in VIEWERS_ALLOWED_APPS
        and p.codename.split("_")[0] in VIEWERS_ALLOWED_ACTIONS
    ]
    viewers_group.permissions.set(viewer_perms)

    opencrow, created = User.objects.get_or_create(username=OPENCROW_USERNAME)
    if created:
        opencrow.set_unusable_password()
        opencrow.save()
        logger.info("Created opencrow user")
    opencrow.groups.set([viewers_group])

    # Grant object-level view+change on all existing objects
    for model, perms in [
        (Document, ("view_document", "change_document")),
        (Tag, ("view_tag", "change_tag")),
        (Correspondent, ("view_correspondent", "change_correspondent")),
        (DocumentType, ("view_documenttype", "change_documenttype")),
        (StoragePath, ("view_storagepath", "change_storagepath")),
    ]:
        for obj in model.objects.all():
            for perm in perms:
                assign_perm(perm, opencrow, obj)

    logger.info(
        "opencrow: %d model-level permissions, object perms granted", len(viewer_perms)
    )


class PaperlessPermsConfig(AppConfig):
    name = "paperless_perms"
    verbose_name = "Paperless permission management"

    def ready(self) -> None:
        from django.contrib.auth.models import User
        from documents.models import (
            Correspondent,
            Document,
            DocumentType,
            StoragePath,
            Tag,
        )

        for model in (Document, Tag, Correspondent, DocumentType, StoragePath):
            post_save.connect(_grant_opencrow_object_perms, sender=model)
        post_save.connect(_add_remote_user_to_editors, sender=User)

        try:
            _setup_editors_group()
            _setup_opencrow_user()
        except Exception:
            logger.exception(
                "Failed initial permission setup (will retry on next restart)"
            )
