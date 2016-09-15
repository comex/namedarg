import sys, re, os, subprocess
version = sys.argv[1]
crates_to_include = ['namedarg_hack', 'namedarg_rustc_macro', 'namedarg']
main_crate = 'namedarg'
check_git_clean = False

crate_paths = [os.path.realpath(crate) for crate in crates_to_include]
subprocess.check_call('rm -rf release-tmp', shell=True)
os.mkdir('release-tmp')
os.mkdir('release-tmp/.cargo')
cargo_config = 'paths = [%s]' % ', '.join('"%s"' % path for path in crate_paths)
open('release-tmp/.cargo/config', 'w').write(cargo_config)

existing = subprocess.check_output('git tag -l %s' % version, shell=True).strip()
if existing:
    raise Exception('git tag already exists for %s' % version)

head = subprocess.check_output('git rev-parse HEAD', shell=True).strip()

print 'To reset:'
print
print '   git checkout master && git reset --hard %s' % head
print '   git tag -d %s' % version
print

if check_git_clean:
    status = subprocess.check_output('git status --porcelain', shell=True).strip()
    if status != '':
        raise Exception('git not clean:\n' + status)
def sub(a, b, data, must_sub=True):
    if must_sub and not re.search(a, data):
        raise Exception("couldn't replace %r with %r in:\n%s" % (a, b, data))
    return re.sub(a, b, data)
for crate in crates_to_include:
    cargo_toml = crate + '/Cargo.toml'
    data = open(cargo_toml).read()
    data = sub(r'(name = "%s"\nversion = )"[^"]+"' % crate,
               r'\1"%s"' % version,
               data)
    data = sub(r'(version = )"[^"]+"(\npath = ")',
               r'\1"= %s"\2' % version,
               data,
               must_sub=('[dependencies.namedarg' in data))
    with open(cargo_toml, 'w') as fp:
        fp.write(data)

print 'cd release-tmp'
os.chdir('release-tmp')
for crate in crates_to_include:
    cmd = 'cargo publish --dry-run --allow-dirty --manifest-path ../%s/Cargo.toml' % crate
    print '>>', cmd
    subprocess.check_call(cmd, shell=True)
