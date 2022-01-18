#!/bin/sh
set -eux -o pipefail
shopt -s nullglob
systemctl stop containerd k3s
find /sys/fs/cgroup/systemd/system.slice/containerd.service* /sys/fs/cgroup/systemd/kubepods* /sys/fs/cgroup/kubepods* -name cgroup.procs | \
    xargs -r cat | xargs -r kill -9
mount | awk '/\/var\/lib\/kubelet|\/run\/netns|\/run\/containerd/ {print $3}' | xargs -r umount
dataset=$((grep /var/lib/containerd/io.containerd.snapshotter.v1.zfs /proc/mounts || :) | awk '{print $1}')
if [[ -n "$dataset" ]]; then
    zfs destroy -R "$dataset"
fi
rm -rf /var/lib/rancher/ /var/lib/containerd /etc/rancher /run/containerd/ /var/lib/cni/
systemctl start k3s
