d2f = function(f, x, h) {
  n = length(x)
  H = matrix(data = 0, nrow = n, ncol = n)
  D = diag(n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      e_i = D[i,]
      e_j = D[j,]
      if (i == j)
      {
        H[i,j] = ( f(x+h*e_i) - 2*f(x) + f(x-h*e_i) ) / (h^2)
      }
      else
      {
        H[i,j] = (f(x+h*e_i+h*e_j) - f(x+h*e_i-h*e_j) - f(x-h*e_i+h*e_j) + f(x-h*e_i-h*e_j)) / (4 * h^2)
      }
    }
  }
  return(H)
}

#Implementacja metody Newtona
newton_nD = function(f, x, h, tol) {
  x_opt = x
  #Zdefiniowanie listy z przechowywanymi wynikami
  resu = list()
  resu$x_opt = x_opt;
  resu$f_opt = f(x_opt);
  resu$x_hist = x_opt; #!!! Bedziemy laczyc rbind bo n wymiarow
  resu$f_hist[1] = f(x_opt);
  resu$iter = 1;
  k = 2;
  finished = FALSE  
  while (finished == FALSE) {
    x_edge = x_opt - numGradient(f, x_opt, h) %*% solve(d2f(f, x_opt, h)) ###jedyna roznica do newton_1D; solve daje macierz odwr
    resu$x_hist = rbind(resu$x_hist, x_edge) #!!!
    resu$f_hist[k] = f(x_edge)
    resu$iter = k;
    k = k + 1
    if (f(x_edge) - f(x_opt) < 0 && abs(sum((x_edge - x_opt)^2)) > tol) {
      x_opt = x_edge
      resu$x_opt = x_opt
      resu$f_opt = f(x_opt)
    } else {
      return(resu)
    }
  }
}

