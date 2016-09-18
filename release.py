import sys, re, os, subprocess
version = sys.argv[1]
crates_to_include = ['namedarg_hack', 'namedarg_rustc_macro', 'namedarg']
main_crate = 'namedarg'
check_git_clean = True

if check_git_clean:
    status = subprocess.check_output('git status --porcelain', shell=True).strip()
    if status != '':
        raise Exception('git not clean:\n' + status)

existing = subprocess.check_output('git tag -l %s' % version, shell=True).strip()
if existing:
    raise Exception('git tag already exists for %s' % version)

head = subprocess.check_output('git rev-parse HEAD', shell=True).strip()

print 'To reset:'
print
print '   git checkout master && git reset --hard %s' % head
print '   git tag -d %s' % version
print

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

subprocess.check_call('git commit --allow-empty -a -m "[auto] %s"' % version, shell=True)
subprocess.check_call('git tag %s' % version, shell=True)

for crate in crates_to_include:
    cmd = 'cargo publish --manifest-path %s/Cargo.toml' % crate
    print '>>', cmd
    subprocess.check_call(cmd, shell=True)
    subprocess.check_call('sleep 3', shell=True) # ... without this i've had later publishes fail to see earlier ones
