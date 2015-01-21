import pickle
import sys

with open(sys.argv[1], "rb") as f:
    dic = pickle.load(f)

lst = []
for i in dic:
    lst.append( (i, dic[i]) )

lst.sort(key=lambda x:x[1])
lst.reverse()
labels = []
values = []
print(lst[0])
for i in lst[1:12]:
    server = i[0].strip()
    if(server == ''): continue
    labels.append(i[0].strip())
    values.append(i[1])
import matplotlib.pyplot as plt
import numpy as np
plt.style.use('ggplot')
width = 0.5
plt.bar(np.arange(10)-width/2, values, width)
plt.xticks(np.arange(10), labels, rotation=45, ha='right') 
plt.xlim(-1,10)
#plt.show()
fig = plt.gcf()
fig.savefig('hist.pdf', bbox_inches='tight')
