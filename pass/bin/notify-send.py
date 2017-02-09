#! /usr/bin/env nix-shell
#! nix-shell -i python -p python35Packages.pygobject3

import gi.repository
from gi.repository import Gio, GLib
import argparse
import sys

def send_notification(title, text, id, timeout, urgency):
    session_bus                = Gio.BusType.SESSION
    cancellable                = None
    proxy_property             = 0
    interface_properties_array = None
    destination                = "org.freedesktop.Notifications"
    path                       = "/org/freedesktop/Notifications"
    interface                  = destination
    method                     = "Notify"
    application_name           = "pynotifysend"
    icon                       = ""
    actions_list               = []
    hints_dict                 = { "urgency": GLib.Variant('y', urgency) }
    dbus_timeout               = -1

    connection = Gio.bus_get_sync(session_bus, cancellable)
    notify = Gio.DBusProxy.new_sync( connection, proxy_property, interface_properties_array,
                                    destination, path, interface, cancellable)
    args = GLib.Variant('(susssasa{sv}i)', ( application_name, id, icon, title, text, actions_list, hints_dict, timeout))
    result = notify.call_sync(method, args, proxy_property, dbus_timeout, cancellable)
    id = result.unpack()[0]
    return id

def close_notification(id):
    session_bus                = Gio.BusType.SESSION
    cancellable                = None
    proxy_property             = 0
    interface_properties_array = None
    destination                = "org.freedesktop.Notifications"
    path                       = "/org/freedesktop/Notifications"
    interface                  = destination
    method                     = "CloseNotification"
    dbus_timeout               = -1

    connection = Gio.bus_get_sync(session_bus, cancellable)
    notify = Gio.DBusProxy.new_sync( connection, proxy_property, interface_properties_array, destination, path, interface, cancellable)
    args = GLib.Variant('(u)', ( id, ))
    result = notify.call_sync(method, args, proxy_property, dbus_timeout, cancellable)


def main():
    parser = argparse.ArgumentParser(description="notify-send, but better")
    parser.add_argument('title', metavar='TITLE', type=str, nargs='?', help="title of message")
    parser.add_argument('text', metavar='TEXT', type=str, nargs='?', help="text of message")
    parser.add_argument('-p', '--print-id', action='store_true', help="print message id")
    parser.add_argument('-r', '--replace-id', metavar='ID', type=int, default=0, help="replace previous message")
    parser.add_argument('-c', '--close-id', metavar='ID', type=int, help="close previous message")
    parser.add_argument('-t', '--timeout', metavar='MILLIS', type=int, default=5000, help="auto-dismiss after millis")
    parser.add_argument('-u', '--urgency', metavar='URGENCY', type=int, default=1, help="0 = low, 1 = normal, 2 = critical")

    args = parser.parse_args()
    if args.close_id:
        close_notification(args.close_id)
    elif args.title and args.text:
        id = send_notification( args.title, args.text, args.replace_id, args.timeout, args.urgency )
        if args.print_id:
            print(id)
    else:
        print("Error: Must provide either -c <id> or title and text")
        sys.exit(1)

if __name__ == "__main__":
   main()
