#! /usr/bin/env nix-shell
#! nix-shell -i python -p python35Packages.pyyaml pass xdotool xclip rofi

"""
Copy password from [pass](https://www.passwordstore.org) store.

Allows single paste of username, then single paste of password.

Most feedback is through libnotify Notifications.

Can automatically infer hostname from current window title (set
ui.window-title-format to `Qute|{host}`), or accept command line argument.

Hosts are stored under a by-host/ directory in password store.

Password store is mounted on-demand from a disk image encrypted with LUKS and
GPG. It is recommended to auto-close it when the screen is locked or smartcard
is removed or after a timeout.

Double-encrypt keeps metadata encrypted at rest, and all but requested password
encrypted while in use.

Will automatically import from lastpass.csv if present in pass store.
"""

import yaml
import subprocess
import sys
import csv
import os

def HOME(path): return os.path.join( os.environ["HOME"], path )

password_store       = HOME(".password-store")
password_img         = HOME("password-container.img")
password_key         = HOME("password-container.key.gpg")
dmcrypt_mapping_name = "passwords"
dmcrypt_device       = os.path.join( "/dev/mapper", dmcrypt_mapping_name )

def run(*args, **kwargs):
    return subprocess.run( [str(a) for a in args]
                         , universal_newlines=True # Use strings not bytes
                         , stdout=subprocess.PIPE  # Capture stdout
                         , check=True              # Throw exception on non-zero return
                         , **kwargs ).stdout.strip()

def sudo(*args, **kwargs):
    # Use -A and SUDO_ASKPASS to make sudo use a GUI dialog for password entry.
    sudo_env = os.environ.copy()
    sudo_env["SUDO_ASKPASS"] = "askpass-pinentry.sh"
    args = ["sudo", "-A"] + args
    return run( *args, env=sudo_env, **kwargs )

def notify(title, text, id_to_replace=None, timeout=-1, urgency=1):
    id_option = ["-r", id_to_replace] if id_to_replace else []
    p = run( "notify-send.py",
             *id_option,
             "-t", timeout,
             "-u", urgency,
             "-p", # print notification id
             title, text )
    return int(p)

def notify_close(id_to_close):
    return run( "notify-send.py", "-c", id_to_close )

def get_active_window_title():
    return run( "xdotool", "getactivewindow", "getwindowname" )

def rofi_select_index(opts):
    p = run( "rofi",
             "-dmenu",         # Read options from STDIN
             "-p", "user",     # Prompt with "user" instead of "dmenu"
             "-format", "i",   # Output index of selected item
             input = "\n".join(opts) )
    return int(p)

def wait_for_single_paste(s):
    run( "xclip",
         "-quiet",                  # Run in the foreground
         "-selection", "CLIPBOARD", # Wait for request for clipboard
         "-l", "1",                 # Wait for one paste request, then close
         input = s)

def extract_host_name(s):
    if s.startswith('Qute|'):
        return s.split("|")[1]

def host_to_pass_key(host):
    return os.path.join("by-host", host)

def pass_show(name):
    msg_id = notify(name, "Press Yubikey to decrypt", urgency=2)
    try:     return run( "pass", "show", name )
    except:  return None
    finally: notify_close(msg_id)

def pass_show_host_yaml(host):
    s = pass_show( host_to_pass_key(host) )
    if s: return [doc for doc in yaml.safe_load_all(s)]

def pass_insert(name, value):
    return run( "pass", "insert", "-m", name, input=value )

def pass_insert_host_yaml(host, yaml_docs):
    value = yaml.safe_dump_all(
        yaml_docs,
        # use multiline "block" style instead of {a:b} ("flow" style)
        default_flow_style=False
    )

    pass_insert( host_to_pass_key(host), value )

def lastpass_import(host):
    msg_id = notify("pass", "No host yet, looking up LastPass password", urgency=0)

    try:
        lpassinfo = pass_show("lastpass.csv")

        if not lpassinfo:
            notify_send("pass", "No lastpass.csv, I can't help you :(", urgency=2)
            return

        info = []
        for row in csv.reader(lpassinfo.split("\r\n")):
            if len(row)>0 and host in row[0]:
                info.append( {'user': row[1], 'pass':row[2], 'lp_url': row[0], 'lp_extra':row[3], 'lp_name':row[4], 'lp_grouping':row[5], 'lp_fav':row[6]} )

        if info:
            docs = [info[0]['pass'], info]
            pass_insert_host_yaml( host, docs )
            return docs

    finally:
        notify_close(msg_id)

def has(d,k):
    return k in d and d[k] and not d[k].isspace()

def to_description(d):
    return next( (d[k] for k in ["user","lp_name","lp_url"] if has(d,k)), "Unknown" )

def make_sure_mapper_present():
    if not os.path.exists( dmcrypt_device ):
        msg_id = notify("pass", "Decrypting volume - press Yubikey to allow", urgency=2)

        try:
            # I don't like getting in the middle, but GPG might ask for a pin and
            # we need to wait for it, because sudo's pinentry would force cancel it.
            key = run("gpg2", "--decrypt", password_key)
            sudo("cryptsetup", "luksOpen", "--key-file=-", password_img, dmcrypt_mapping_name, input=key)
            key='' # Probably ineffective, but we can try

        finally:
            notify_close(msg_id)

def make_sure_store_mounted():
    if not os.listdir(password_store):
        msg_id = notify("pass", "Mounting password store", urgency=0)
        try:
            make_sure_mapper_present()
            sudo("mount", "-onoexec,nosuid,nodev", dmcrypt_device, password_store)
        finally:
            notify_close(msg_id)

def main():
    host = sys.argv[1] if len(sys.argv)>1 else extract_host_name(get_active_window_title())
    if host is None:
        notify("pass", "Can't figure out host name to look up", urgency=2)
        sys.exit(1)

    make_sure_store_mounted()

    docs = pass_show_host_yaml(host) or lastpass_import(host)

    info = None

    if not docs:
        notify(host, "Couldn't find any login info :(", urgency=2)
        sys.exit(1)
    elif len(docs) == 1:
        info = {"user": docs}[0]
    else:
        idx = 0
        if len(docs[1]) > 1:
            idx = rofi_select_index( [ to_description(x) for x in docs[1] ] )

        info = docs[1][idx]

    for field in ["user", "pass"]:
        if has(info, field):
            msg_id = notify(host, field + " in clipboard", urgency=2)
            wait_for_single_paste(info[field])
            notify_close(msg_id)

    notify(host, "Done!", timeout=1000, urgency=0)

if __name__ == "__main__":
   main()
