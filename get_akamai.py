# get akamaighost 
import sys
import ipaddress
import collections 
import re

akamai_servers = []
cnt = 0
with open(sys.argv[1]) as f:
    for line in f:
        sp = line.strip().split(" ", 7)
        ipstr = sp[5][:-1]
        packedIP = int(ipaddress.IPv4Address(ipstr))
        protocol = sp[6][1:-1]
        if protocol != 'http': continue
        banner = sp[7].encode().decode('unicode-escape')
        sp = banner.splitlines()
        server = "not specified"
        for token in sp:
            if token.lower().startswith("server:"):
                server = re.split('[: ]', token, 1)[1]
        if server.strip() == "AkamaiGHost":
            akamai_servers.append(packedIP)

import pickle
with open("akamai.pcl", "wb") as f:
    pickle.dump(akamai_servers, f)
