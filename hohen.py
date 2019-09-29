import numpy as np
import matplotlib.pyplot as plt
import math
"""size = (50,50)
spread = 0.05
start = 2*spread*(np.random.random((50,50))-.5)

def laplace(mat,x,y):
    return 


plt.imshow(start)
plt.colorbar()
plt.show()"""

def swifthohen(epsilon, g):
    stop=0;

    L=40*math.pi;
    N=512;
    a0=(4*epsilon/3)**.5;
    noise = 0.2;

    t=0;
    dt=0.3/epsilon;
    U=2*noise*(np.random.random((N,N))-.5)
    #Ut=[t*np.log(np.absolute(U))*np.log(a0)]
    q=np.zeros((N,1));
    for k in range(N):
      q[k]=(2*(k-1)/N-1)*math.pi*N/L;

    Q=U
    for k in range(N):
      for l in range(N):
        Q[k,l]=1/(1-dt*(epsilon-(1-(q[k]**2+q[l]**2))**2));

    n=0
    while t<100/epsilon and stop==0:
      n=n+1;
      u=np.fft.fftshift(np.fft.fft2(U));
      t=t+dt;
      u=u*Q;
      U=np.real(np.fft.ifft2(np.fft.ifftshift(u)));
      U=U+dt*g(U);
      #Ut.append(t*np.log(np.absolute(U))*np.log(a0))

      if n%30==0:
        plt.imshow(U)
        plt.colorbar()
        plt.show()
g = lambda x: -0.02*x**2 - x**3
swifthohen(.01,g)
