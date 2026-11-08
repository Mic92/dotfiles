// Applies the configuration in `cfg` (prepended by nix-uci) on an OpenWrt
// device: packages, files, uci settings, root password, service restarts.
'use strict';

let uci = require('uci');
let fs = require('fs');

function q(s) {
	return "'" + replace(s, "'", "'\\''") + "'";
}

function sh(cmd) {
	let rc = system(cmd);
	if (rc != 0)
		die(sprintf('command failed (%d): %s\n', rc, cmd));
}

function mkdirp(path) {
	let cur = '';
	for (let part in split(path, '/')) {
		if (part == '')
			continue;
		cur += '/' + part;
		if (!fs.stat(cur) && !fs.mkdir(cur, 0755))
			die(sprintf('mkdir %s: %s\n', cur, fs.error()));
	}
}

// Write via rename so readers never see a partial file and secrets are
// never briefly world-readable.
function write_file(f) {
	mkdirp(fs.dirname(f.path));
	let tmp = f.path + '.nix-uci-tmp';
	fs.unlink(tmp);
	let fh = fs.open(tmp, 'w', f.mode);
	if (!fh)
		die(sprintf('open %s: %s\n', tmp, fs.error()));
	fh.write(f.content);
	fh.close();
	fs.chmod(tmp, f.mode);
	if (!fs.rename(tmp, f.path))
		die(sprintf('rename %s: %s\n', f.path, fs.error()));
}

function option_value(v) {
	if (type(v) == 'array')
		return map(v, option_value);
	if (type(v) == 'bool')
		return v ? '1' : '0';
	return '' + v;
}

function set_options(c, conf, sid, section) {
	for (let opt, val in section) {
		if (opt == '_type')
			continue;
		if (!c.set(conf, sid, opt, option_value(val)))
			die(sprintf('uci set %s.%s.%s: %s\n', conf, sid, opt, c.error()));
	}
}

function apply_config(c, conf, sections) {
	if (!fs.stat('/etc/config/' + conf))
		fs.writefile('/etc/config/' + conf, '');
	c.load(conf);

	for (let name, sec in sections) {
		if (type(sec) == 'array') {
			// list sections: `name` is the section type, replace all of them
			let old = [];
			c.foreach(conf, name, s => { push(old, s['.name']); });
			for (let sid in old)
				c.delete(conf, sid);
			for (let item in sec) {
				let sid = c.add(conf, item._type ?? name);
				if (!sid)
					die(sprintf('uci add %s %s: %s\n', conf, name, c.error()));
				set_options(c, conf, sid, item);
			}
		} else {
			if (!sec._type)
				die(sprintf('%s.%s has no _type\n', conf, name));
			c.delete(conf, name);
			if (!c.set(conf, name, sec._type))
				die(sprintf('uci set %s.%s: %s\n', conf, name, c.error()));
			set_options(c, conf, name, sec);
		}
	}
}

// Packages first, so their default configs cannot overwrite ours.
if (length(cfg.packages))
	sh('apk update >/dev/null && apk add ' + join(' ', map(cfg.packages, q)) + ' >/dev/null');

for (let d in cfg.replace_dirs)
	sh('rm -rf ' + q(d));
for (let f in cfg.files)
	write_file(f);

let c = uci.cursor();
let confs = keys(cfg.settings);
try {
	for (let conf in confs)
		apply_config(c, conf, cfg.settings[conf]);
} catch (e) {
	for (let conf in confs)
		c.revert(conf);
	die(sprintf('applying uci settings failed, reverted: %s\n', e.message));
}
for (let conf in confs)
	if (!c.commit(conf))
		die(sprintf('uci commit %s: %s\n', conf, c.error()));

if (cfg.root_password != null)
	sh(sprintf("printf '%%s\\n%%s\\n' %s %s | passwd root >/dev/null", q(cfg.root_password), q(cfg.root_password)));

// Detached: restarting the network may change the address we are
// connected to, which would otherwise kill this session.
let restarts = join('; ', map(cfg.restart_services, s => '/etc/init.d/' + q(s) + ' restart'));
sh('(sleep 1; reload_config; ' + (length(restarts) ? restarts : ':') + ') >/dev/null 2>&1 </dev/null &');
print('applied\n');
