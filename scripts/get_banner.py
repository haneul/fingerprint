import sys
import ipaddress

pool = set()

cnt = 0
if True:
    for line in sys.stdin:
        cnt += 1
        #if cnt == 500: break
        if line.startswith("Discovered"):
            #parsing Discovered open port 80/tcp on 207.54.20.4
            sp = line.split()
            ipstr = sp[-1]
            packedIP = int(ipaddress.IPv4Address(ipstr))
            pool.add(packedIP)
        #else:
            #fw.write(line)
with open("res.txt", "w") as fw:
    fw.write(str(len(pool)))
