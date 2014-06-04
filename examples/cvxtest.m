clear all; close all;

a = [0 2 0 0;
    -3 0 0 0;
     0 0 1 0;
     0 0 0 4];
b = [4  8 -1 0;
     0 -2  1 0;
     9 -3  0 3;
     0  0  2 0];

c = [1; 2; 3; 4];

cvx_begin
    variables x(4) y(4);
    y <= 2;
    x <= -3;
    minimize(norm(a*x) + norm(b*y) + norm(c + x - y));
cvx_end
