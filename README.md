# portfolio-constant-correlation-ss
Constant Correlation Model with Short Sale

Constant Correlation works the same way as the Single Index Model functions
only that it is not required to input the market information since they are not
part of the calculation. In our case where short sales are allowed, we type the following:

port.cc.ss(data=ret.mon,rf=0.001,lint=TRUE)
