"""Manage Paperless-ngx users and groups.

Loaded as a Django app via PAPERLESS_APPS. On startup it creates an
"editors" group (view/add/change/delete on all models), assigns all remote
users (containing "@") to it, and installs a post_save signal so new remote
users are handled automatically in every worker process.

Django model imports are deferred because the app registry is not yet
populated at module import time — top-level imports would raise
AppRegistryNotReady.
"""

import logging

from django.apps import AppConfig
from django.db.models.signals import post_save

logger = logging.getLogger(__name__)


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


class PaperlessPermsConfig(AppConfig):
    name = "paperless_perms"
    verbose_name = "Paperless permission management"

    def ready(self) -> None:
        from django.contrib.auth.models import User

        post_save.connect(_add_remote_user_to_editors, sender=User)

        try:
            _setup_editors_group()
        except Exception:
            logger.exception(
                "Failed initial permission setup (will retry on next restart)"
            )
