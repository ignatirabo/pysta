# CVE-2018-1000802
def _call_external_zip(base_dir, zip_filename, verbose, dry_run):
    # XXX see if we want to keep an external call here
    if verbose:
        zipoptions = "-r"
    else:
        zipoptions = "-rq"
    # from distutils.errors import DistutilsExecError
    # from distutils.spawn import spawn
    cmd = ["zip", zipoptions, zip_filename, base_dir]
    ret = distutils.spawn.spawn(cmd, 1, verbose, dry_run)
    return ret
base_dir = "/Users/ignacio"
zip_filename = input("Input filename: ")
y = input()

if y > 0:
  x = _call_external_zip(base_dir, zip_filename, True, False)
else:
  x = 5
