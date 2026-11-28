#!/usr/bin/env python3
# Every library which a Mach-O file of a macOS application loads is in the
# application or is one of macOS (#972): an absolute path in /usr/lib or
# /System, or an @rpath, @loader_path or @executable_path name which leads
# to a file inside the application. Checked on the files themselves, not by
# running them: on the machine of the build, a framework which macdeployqt
# did not copy is still found through the rpath of the Qt of the build.
#
#   check-app-libs.py <application>.app...
#
# An @rpath name is looked up in the rpaths of the file and in those of the
# executable which loads it (the main executable for the frameworks and the
# plug-ins, the file itself for the other executables); the rpaths outside
# the application do not count.

import os
import subprocess
import sys


def out(*cmd):
    return subprocess.run(cmd, capture_output=True, text=True, check=True).stdout


def is_macho(path):
    if os.path.islink(path) or not os.path.isfile(path):
        return False
    return "Mach-O" in out("file", "-b", path)


def is_executable(path):
    return "executable" in out("file", "-b", path)


def install_ids(path):
    # otool -D: the file name, then the install name of each architecture
    return {l.strip() for l in out("otool", "-D", path).splitlines()[1:]}


def dependencies(path):
    deps = []
    for line in out("otool", "-L", path).splitlines():
        if line.startswith("\t"):
            name = line.strip().rsplit(" (compatibility", 1)[0]
            if name not in deps:
                deps.append(name)
    return [d for d in deps if d not in install_ids(path)]


def rpaths(path):
    found, lines = [], out("otool", "-l", path).splitlines()
    for i, line in enumerate(lines):
        if line.strip() == "cmd LC_RPATH":
            for l in lines[i + 1:i + 3]:
                l = l.strip()
                if l.startswith("path "):
                    p = l[5:].rsplit(" (offset", 1)[0]
                    if p not in found:
                        found.append(p)
    return found


def expand(name, loader, executable):
    if name.startswith("@loader_path/"):
        return os.path.join(os.path.dirname(loader), name[len("@loader_path/"):])
    if name.startswith("@executable_path/"):
        return os.path.join(os.path.dirname(executable),
                            name[len("@executable_path/"):])
    return name


def inside(path, app):
    real = os.path.realpath(path)
    return real == app or real.startswith(app + os.sep)


def check(app):
    app = os.path.realpath(app)
    contents = os.path.join(app, "Contents")
    name = out("plutil", "-extract", "CFBundleExecutable", "raw",
               os.path.join(contents, "Info.plist")).strip()
    main = os.path.join(contents, "MacOS", name)
    files = sorted(os.path.join(d, f) for d, _, fs in os.walk(contents)
                   for f in fs if is_macho(os.path.join(d, f)))
    main_rpaths = rpaths(main)
    errors = []
    for f in files:
        executable = f if is_executable(f) else main
        own = rpaths(f)
        search = own + ([] if executable == f else main_rpaths)
        for d in dependencies(f):
            if d.startswith("/"):
                if not d.startswith(("/usr/lib/", "/System/")):
                    errors.append(f"{f}: {d} outside the application")
                continue
            if d.startswith("@rpath/"):
                rest = d[len("@rpath/"):]
                cands = [os.path.join(expand(r, f, executable), rest)
                         for r in search]
                cands = [c for c in cands if not c.startswith("@")]
            else:
                cands = [expand(d, f, executable)]
            if not any(os.path.exists(c) and inside(c, app) for c in cands):
                errors.append(f"{f}: {d} not in the application")
        outside = [r for r in own if not r.startswith("@")
                   and not inside(r, app)]
        if outside:
            print(f"note: {os.path.relpath(f, app)}: rpath outside the "
                  f"application: {', '.join(outside)}")
    for e in errors:
        print("error: " + e)
    print(f"{app}: {len(files)} Mach-O files, "
          f"{'no library outside the application' if not errors else str(len(errors)) + ' missing'}")
    return not errors


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__ or "usage: check-app-libs.py <application>.app...")
    ok = all([check(a) for a in sys.argv[1:]])
    sys.exit(0 if ok else 1)
