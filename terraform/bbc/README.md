# For shannan:

0. Change directory

```
$ cd $HOME/.homesick/repos/dotfiles/terraform/bbc
```

1. Create the droplet

```
$ terragrunt apply
```

2. Connect to it

```
$ sshuttle -r root@bbc.lekwati.com 0/0 ::/0
```

3. Enjoy

4. Press Ctrl-C to stop sshutttle

5. Destroy

```
$ terragrunt destroy
```
