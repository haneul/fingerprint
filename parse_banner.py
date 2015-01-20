# parsing banner file
import sys
import ipaddress
import collections 
import re

protocol_dic = collections.defaultdict(int)
server_dic = collections.defaultdict(int)
cnt = 0
with open(sys.argv[1]) as f:
    for line in f:
        sp = line.strip().split(" ", 7)
        ipstr = sp[5][:-1]
        packedIP = int(ipaddress.IPv4Address(ipstr))
        protocol = sp[6][1:-1]
        if protocol == "title": continue
        protocol_dic[protocol] += 1
        if protocol != 'http': continue
        banner = sp[7].encode().decode('unicode-escape')
        sp = banner.splitlines()
        server = "not specified"
        for token in sp:
            if token.lower().startswith("server:"):
                server = re.split('[: ]', token, 1)[1]
        server_dic[server] += 1 

import pickle
with open("protocol.pcl", "wb") as f:
    pickle.dump(protocol_dic, f)
with open("server.pcl", "wb") as f:
    pickle.dump(server_dic, f)

print("protocol")
for i in protocol_dic:
    print(i + "\t" + str(protocol_dic[i]))
print()
print("server")
for i in server_dic:
    print(i + "\t" + str(server_dic[i]))


