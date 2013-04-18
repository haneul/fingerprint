#!/usr/bin/python

import os
import sys
import commands
import optparse
import time
import socket

from os.path import *

TOP = os.path.dirname(__file__)

def hostname():
    return commands.getoutput("hostname").strip()

def get_me():
    me = hostname()
    if me not in get_pcs():
        print >> sys.stderr, "%s isn't recognized" % me
        exit(1)
    return get_pcs().index(me)

def get_pcs():
    if not hasattr(get_pcs, "_pcs"):
        get_pcs._pcs = \
            [l.strip() for l in open(join(TOP, "planet/machines"))]
    return get_pcs._pcs

def check_status():
    def do_check(p):
        # statues:
        #   B: booting = ps aux | grep installer
        #   R: running = ps aux | grep scan
        #   D: done    = check lastest log entry
        #   
        # #sock
        #   netstat -t | wc -l
        #
        pass
    for p in get_pcs():
        # create a thread
        pass
    # join
    return

def assigned_ip(me, machines, testing=False):
    T = len(machines)
    S = 255 / T

    # need some intelligence here,
    #  for example, class C might have lots of open machines?

    from_ip = "%s.0.0.0" % (S*me)
    to_ip   = "%s.255.255.255" % (S*(me+1)-1)

    # in case we are testing, tweak it for running a few secs
    if testing:
        to_ip = "%s.0.0.100" % (S*me)

    return (from_ip, to_ip)

def mkdirp(pn):
    try:
        os.mkdir(join(TOP, pn))
    except:
        pass

def log(entry):
    mkdirp("out")
    
    e = "[%s] %s" % (time.ctime(), entry)
    fd = open(join(TOP, "out/stat"), "w")
    fd.write(e)
    fd.close()
    fd = open(join(TOP, "out/log"), "a+")
    fd.write(e + "\n")
    fd.close()

def do_scan(opts):
    now = time.strftime("%Y%m%d")
    
    mkdirp("out")
    mkdirp("out/%s" % now)
    HOST = opts.server_address
    PORT = opts.server_port
    
    #
    # TOP/out/now/ip-ip
    # TOP/out/log
    # TOP/out/stat
    #
    while True:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            sock.connect((HOST, PORT))
            sock.sendall("GET %s\n" % hostname())
            # Receive data from the server and shut down
            received = sock.recv(1024)
            targetIP = int(received.strip().split()[1])
            if(targetIP == -1): break
        finally:
            sock.close()
        (from_ip, to_ip) = ("%d.0.0.0" % targetIP, "%d.255.255.255" % targetIP)
        out  = join(TOP, "out/%s/%s-%s" % (now, from_ip, to_ip))
        print(out)
        scan = join(TOP, "dist/build/scan/scan")
        log("BEG %s - %s" % (from_ip, to_ip))
        os.system("%s %s %s %s 2>&1 >> %s" \
                % (scan, opts.nc, from_ip, to_ip, out))
        log("END %s - %s" % (from_ip, to_ip))
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            sock.connect((HOST, PORT))
            sock.sendall("DONE %s (%d)\n" % (hostname(), targetIP))
            # Receive data from the server and shut down
            received = sock.recv(1024)
        finally:
            sock.close()
    
if __name__ == '__main__':
    parser = optparse.OptionParser(__doc__.strip() if __doc__ else "")
    parser.add_option("-n", "--NC",
                      help="#connections (fd)", type="int",
                      dest="nc", default=1000)
    parser.add_option("-s", "--check-status",
                      help="check server status", action="store_true",
                      dest="check_status", default=False)
    parser.add_option("-c", "--check-ip",
                      help="check assigned ips", action="store_true",
                      dest="check_ip", default=False)
    parser.add_option("-r", "--reclaim",
                      help="reclaim logs", action="store_true",
                      dest="reclaim", default=False)
    parser.add_option("-t", "--testing",
                      help="testing", action="store_true",
                      dest="testing", default=False)
    parser.add_option("-H", "--host",
                      help="server_address",
                      dest="server_address", default="hydralisk.cs.washington.edu")
    parser.add_option("-p", "--port",
                      help="server_port", type="int",
                      dest="server_port", default=9999)
    (opts, args) = parser.parse_args()
    
    if opts.check_ip:
        pcs = get_pcs()
        for (i, m) in enumerate(pcs):
            print "%-40s: %15s - %15s" \
                % ((m,) + assigned_ip(i, pcs, opts.testing))
        exit(0)

    # otherwise, do scan
    do_scan(opts)
    
    
