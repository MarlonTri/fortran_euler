import numpy as np
import matplotlib.pyplot as plt
import matplotlib
from matplotlib import animation
import math
"""size = (50,50)
spread = 0.05
start = 2*spread*(np.random.random((50,50))-.5)

def laplace(mat,x,y):
    return 


plt.imshow(start)
plt.colorbar()
plt.show()"""

fig = plt.figure()

stop=0;
epsilon = 0.11


L=40*math.pi;
N=128;

noise = 0.62;

t=0;
dt=0.1/epsilon;
U=2*noise*(np.random.random((N,N))-.5)
im = plt.imshow(U)
plt.colorbar()
q=np.zeros((N,1))
for k in range(N):
    q[k]=(2*(k-1)/N-1)*math.pi*N/L;

Q=U
for k in range(N):
    for l in range(N):
        Q[k,l]=1/(1-dt*(epsilon-(1-(q[k]**2+q[l]**2))**2));
        
def swifthohen(i):
    skip=1
    
    for k in range(skip):
        U = im.get_array()
        u=np.fft.fftshift(np.fft.fft2(U));

        u=u*Q;
        U=np.real(np.fft.ifft2(np.fft.ifftshift(u)));
        U=U+dt*((1-0.01*i)*U**2-U**3)
        im.set_array(U)
        print(i)
        
    return im

anim = animation.FuncAnimation(fig, swifthohen,
                               frames=10000, interval=1)

plt.show()

