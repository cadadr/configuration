#!/usr/bin/env python3
# install-packages.py --- install packages w/ distro alternatives from input

### Description:

"""install-packages.py

Install packages from a list, with the possibility of specifying
groupings and alternatives.

See doc/packages.list.example for an example input file.

By default only prints out the required command.  See the command
line arguments for how to execute the command instead of merely
printing it out.

"""

### Code:
import argparse
import distro
import functools
import itertools
import operator
import os
import sys

#### Package manager commands and OS data:
_deb_cmd = "apt-get install -y"
package_manager_commands = {
    "debian": _deb_cmd,
    "ubuntu": _deb_cmd,
    "elementary": _deb_cmd,
    "arch": "pacman -Syu"
}

current_distro = distro.id()
like_what = distro.like()

def get_alternative(package, try_similar_distro=False, dist=current_distro):
    """Try to find the alternative package for current distro.

    If try_similar_distro is True, try to find a variant from a
    `similar' distro.  For how that's determined, see the `distro'
    package.

    If not found, return a tuple of abstract name and None.

    """
    if isinstance(package, str):
        return package
    elif isinstance(package, tuple):
        try:
            abstract_name, alternatives = package
            try:
                return alternatives[dist]
            except KeyError:
                for key, alt in alternatives.items():
                    if isinstance(key, tuple) and (dist in key):
                        return alt
                if try_similar_distro:
                    return get_alternative(package, dist=like_what)
                else:
                    return (abstract_name, None)
        except IndexError:
            raise TypeError("Bad package spec")
    else:
        raise TypeError("Not a package name or alternatives tuple")

#### Script:

def get_packages(input, try_similar_distro=False):
    """Get packages from input.

    Return a hash, containing an item `all' which contains a list of
    all packages, and other elements representing the group
    hierarchies and containing lists of relevant packages.

    """
    groups = {"all": []}

    for name, group in input.items():
        if isinstance(group, list):
            for package in group:
                alt = get_alternative(
                    package, try_similar_distro=try_similar_distro)
                groups["all"].append(alt)
                try: groups[name].append(alt)
                except KeyError: groups[name] = [alt]
        # A subgroup:
        elif isinstance(group, dict):
            name = name
            contents = get_packages(group)
            groups["all"].extend(contents.pop("all"))
            for subname, val in contents.items():
                groups[name + "." + subname] = val
        else:
            raise TypeError("Bad group: {}".format(group))

    return groups

def load_file(filename):
    namtab = {}
    # Eval list:
    try:
        exec(compile(open(filename, "rb").read(), filename, 'exec'), namtab)
    except Exception as e:
        raise Exception("Exception while evaluating list file:\n{}".format(e))

    # Return group defs:
    try:
        return namtab["packages"]
    except:
        raise Exception(
            "`{}' does not define variable `packages'".format(filename))

def make_command(packages, selections, try_similar_distro=False,
                 dist=current_distro):
    """Generate a command to install packages.

    Selections is a list of groups to select from.

    Returns a tuple where [0] is the command in a string and [1] is a
    list of unavailable packages.

    """
    try:
        selected = functools.reduce(
            operator.concat, [packages[group] for group in selections])
    except KeyError as e:
        bad_group = e.args[0]
        raise Exception("Unknown group '{}'".format(bad_group))
    # XXX(2019-01-13): the None-check below is redundant, but I may
    # put some information there in the future.
    unavailable = [pkg[0] for pkg in selected
                   if isinstance(pkg, tuple) and pkg[1] is None]
    available = itertools.takewhile(lambda p: isinstance(p, str), selected)
    command = None
    try:
        command = package_manager_commands[dist]
    except KeyError:
        if try_similar_distro:
            try:
                command = package_manager_commands[like_what]
            except KeyError:
                raise Exception(
                    "Unknown distros '{}' or the similar '{}'".format(
                        dist, like_what))
        else:
            raise Exception("Unknown distro '{}'".format(dist))
    else:
        return ("{} {}".format(command, " ".join(available)),
                unavailable)


#### Script:

def usage():
    print("usage: {} LISTFILE GROUP [GROUP...]".format(scriptname, scriptname))
    exit(1)

def parse_args(argv):
    parser = argparse.ArgumentParser(
        description="install packages w/ distro alternatives from input")
    parser.add_argument(
        "input_file", metavar="INPUT", type=str, nargs=1,
        help="package list file")
    parser.add_argument(
        "selections", metavar="GROUP", type=str, nargs="+",
        help="a group name to add to the selections, 'all' means all groups")
    parser.add_argument(
        "-s", "--try-similar-distro", action='store_true',
        help="use packages from similar distro (the one 'distro' package reports)")
    parser.add_argument(
        "-q", "--quiet", action='store_true',
        help="be quiet, i.e. do not report unavailable packages")
    parser.add_argument(
        "-X", "--execute-command", action='store_true',
        help="execute the resulting command after printing it")

    return parser.parse_args(argv[1:])

def main(argv):
    # Parse arguments
    args = parse_args(argv)

    # Process data
    input_file = args.input_file[0]
    selections = args.selections
    similarp = args.try_similar_distro
    execp = args.execute_command
    quietp = args.quiet

    # Check if selections are sane
    if 'all' in selections and len(selections) != 1:
        raise Exception("Group 'all' can not be combined with other selections")
    if len(selections) != len(set(selections)):
        raise Exception("Duplicate groups are not allowed!")

    # Check if input_file is usable
    if os.path.exists(input_file):
        if os.path.isdir(input_file):
            raise Exception("Input file '{}' is a directory".format(
                input_file))
    else:
        raise Exception("Input file '{}' does not exist".format(
            input_file))

    input_data = load_file(input_file)
    packages = get_packages(input_data, try_similar_distro=similarp)

    command, unavailable = make_command(
        packages, selections, try_similar_distro=similarp)

    # Report unavailable
    if len(unavailable) > 0 and not quietp:
        sys.stderr.write("Some packages are unavailable: {}\n".format(
            ", ".join(unavailable)))

    # Output the command
    sys.stdout.write(command)
    sys.stdout.write("\n")

    if execp:
        exit(os.system(command))
    # main() is complete

if __name__ == "__main__":
    main(sys.argv)

