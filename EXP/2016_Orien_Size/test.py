import random
### The date we begin the Chinese registered experiment
SEED = 1200

seq = range(0,41,4)
random.seed(SEED)
random.shuffle(seq)

nr = 37
c = nr/16
nr += seq[c]

'''
if nr <=16:
	nr += seq[0]
elif nr > 16 & nr <= 32:
	nr += seq[1]
elif nr > 32 & nr <= 48:
	nr += seq[2]
elif nr > 48 & nr <= 64:
	nr += seq[3]
elif nr > 64 & nr <= 80:
	nr += seq[4]
elif nr > 80 & nr <= 96:
	nr += seq[5]
else:
	nr += seq[6]
'''
print(nr)
