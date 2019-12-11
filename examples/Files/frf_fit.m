function [SER, fit, dif] = frf_fit(omega, frf, order, weights)
  # - omega: N-element frequency vector (rad/s)
  # - frf: N-element complex-valued frequency response to fit
  # - weights: N-element real-valued weighting vector - when in doubt set to all ones
  
  # Set-Up Options
  opts.relax = 1;
  opts.stable = 1;
  tolLow = 1e-18;
  tolHigh = 1e18;
  
  # Compute the complex-valued frequency vector
  s = 1i * omega;
  
  # Construct an estimate of pole locations
  poles = estimate_poles(omega, order);
  
  # General Initialization
  n = length(omega);
  np = length(poles);
  B = ones(np, 1);
  C = zeros(1, np);
  D = zeros(1, 1);
  x = zeros(np, 1);
  
  # Determine which poles are complex conjugates
  cindex = find_complex(poles);
  
  # Building the system matrices
  Dk = ones(n, np + 1);   # This matrix will be complex-valued
  for i = 1:np
    if cindex(i) == 0
      Dk(:,i) = 1 ./ (s - poles(i));
    elseif cindex(i) == 1
      Dk(:,i) = 1 ./ (s - poles(i)) + 1 ./ (s - poles(i)');
      Dk(:,i+1) = 1i ./ (s - poles(i)) - 1i ./ (s - poles(i)');
    endif
  endfor
  
  # Scaling for the last row for the pole identification process
  scale = norm(weights .* frf) / n;
  
  # ----------------------------------------------------------------------------
  if opts.relax == 1
    AA = zeros(np + 1, np + 1);
    bb = zeros(np + 1, 1);
    Escale = zeros(1, np + 1);
    
    Ac = zeros(n, 2 * (np + 1));  # N -by- 2*(Np+1) - Complex Valued
    offset = np + 1;
    for i = 1:np+1
      Ac(:,i) = weights .* Dk(:,i);
      Ac(:,i+offset) = -weights .* Dk(:,i) .* frf;
    endfor
    A = [real(Ac); imag(Ac); zeros(1, 2 * np + 2)];  # Real-Valued - 2*N+1 -by- 2*(Np+1)
    
    # Establish the integral criteria for sigma
    for i = 1:np+1
      A(2*n+1, offset + i) = real(scale * sum(Dk(:,i)));
    endfor
    [Q, R] = qr(A, 0);  # Form an economy version of Q
    ind1 = np + 2;
    ind2 = 2 * (np + 1);
    R22 = R(ind1:ind2,ind1:ind2); # R22 is upper triangular
    AA(1:np+1,:) = R22;
    bb(1:np+1,1) = Q(size(Q, 1),ind1:ind2)' * n * scale;
    
    for i = 1:np+1
      Escale(i) = 1 / norm(AA(:,i));
      AA(:,i) = Escale(i) .* AA(:,i);
    endfor
    x = AA \ bb;        # AA is Np+1 -by- Np+1 & BB is Np+1 -by- 1
    x = x .* Escale.';  # X is Np+1 -by- 1, and is real-valued
  endif # opts.relax == 1
  
  # Deal with the situation when no relaxation is requested, or D is too small
  if opts.relax == 0 || abs(x(np+1)) < tolLow || abs(x(np+1)) > tolHigh
    AA = zeros(np, np);
    bb = zeros(np, 1);
    if opts.relax == 0
      Dnew = 1;
    else
      if x(np+1) == 0
        Dnew = 1;
      elseif abs(x(np+1)) < tolLow
        Dnew = sign(x(np+1)) * tolLow;
      elseif abs(x(np+1)) > tolHigh
        Dnew = sign(x(np+1)) * tolHigh;
      endif
    endif
    
    Ac = zeros(n, 2 * np + 1);
    Escale = zeros(1, np);
    offset = np + 1;
    for i = 1:np+1
      Ac(:,i) = weights .* Dk(:,i);
    endfor
    for i = 1:np
      Ac(:,i+offset) = -weights .* Dk(:,i) * frf;
    endfor
    bc = Dnew * weights * frf; # N-by-1 - Complex-Valued
    A = [real(Ac); imag(Ac)];
    b = [real(bc); imag(bc)];
    [Q, R] = qr(A, 0);
    ind1 = np + 2;
    ind2 = 2 * np + 1;
    R22 = R(ind1:ind2,ind1:ind2); # R22 is Np-by-Np, and is upper triangular
    AA = R22;
    bb = Q(:,ind1:ind2).' * b;  # Q**T is Np-by-2*N & b is 2*N-by-1
    
    for i = 1:np
      Escale(i) = 1 ./ norm(AA(:,i));
      AA(:,i) = Escale(i) .* AA(:,i);
    endfor
    
    x = AA \ bb;
    x = x .* Escale.';
    x = [x; Dnew];  # X is now Np+1 -by- 1
  endif # opts.relax == 0
  
  # Now generate an estimate for C and D
  C = x(1:np,1).';  # Ensure C is 1-by-Np
  D = x(np+1,1);
  
  # ----------------------------------------------------------------------------
  # Make C complex-valued
  i = 1;
  Cc = zeros(1, np);  # 1-by-Np
  while i <= np
    if cindex(i) == 1
      r1 = C(i);
      r2 = C(i+1);
      Cc(i) = r1 + 1i * r2;
      Cc(i+1) = r1 - 1i * r2;
      i = i + 2;
    else
      i = i + 1;
    endif
  endwhile
  
  # ----------------------------------------------------------------------------
  # Compute the zeros
  m = 0;
  lambda = zeros(np, np); # Np-by-Np - Real-Valued
  for i = 1:np
    m = m + 1;
    if m < np
      pole = poles(m);
      if abs(pole) > abs(real(pole))
        # The pole is complex-valued
        lambda(m+1,m) = -imag(pole);
        lambda(m,m+1) = imag(pole);
        lambda(m,m) = real(pole);
        lambda(m+1,m+1) = real(pole);
        
        B(m,1) = 2.0;
        B(m+1,1) = 0.0;
        
        koko = Cc(m);
        C(m) = real(koko);
        C(m+1) = imag(koko);
        
        m = m + 1;
      endif
    endif
  endfor
  
  zer = lambda - B * C / D; # ZER is Np-by-Np, B is Np-by-1, C is 1-by-Np
  eigenvals = eig(zer);
  
  # Force stability?
  if opts.stable == 1
    unstableIndices = real(eigenvals) > 0.0;
    eigenvals(unstableIndices) = eigenvals(unstableIndices) - 2 * real(eigenvals(unstableIndices));
  endif
  eigenvals = sort(eigenvals);
  
  # ----------------------------------------------------------------------------
  # Sort the eigenvalues, and ensure any real poles end up at the beginning
  for i = 1:np
    for j = i+1:np
      if imag(eigenvals(j)) == 0 && imag(eigenvals(i)) ~= 0
        trans = eigenvals(i);
        eigenvals(i) = eigenvals(j);
        eigenvals(j) = trans;
      endif
    endfor
  endfor
  n1 = 0;
  for i = 1:np
    if imag(eigenvals(i)) == 0
      n1 = i;
    endif
  endfor
  if n1 < np
    eigenvals(n1+1:np) = sort(eigenvals(n1+1:np));
  endif
  eigenvals = eigenvals - 2 * 1i * imag(eigenvals);
  
  # ----------------------------------------------------------------------------
  # Residual Identification
  poles = eigenvals;
  
  # Identify complex poles
  cindex = find_complex(poles);
  
  # ----------------------------------------------------------------------------
  # Compute the new fitting using the new poles estimate
  Ac = zeros(n, np + 1);  # Complex-Valued
  Bc = zeros(n, 1);       # Complex-Valued
  Dk = zeros(n, np);          # Complex-Valued
  
  for i = 1:np
    if cindex(i) == 0
      Dk(:,i) = weights ./ (s - poles(i));
    elseif cindex(i) == 1
      Dk(:,i) = weights .* (1 ./ (s - poles(i)) + 1 ./ (s - poles(i)'));
      Dk(:,i+1) = 1i * weights .* (1 ./ (s - poles(i)) - 1 ./ (s - poles(i)'));
    endif
  endfor
  Ac(:,1:np) = Dk;
  Ac(:,np+1) = weights;
  Bc(:,1) = weights .* frf;
  A = [real(Ac); imag(Ac)];
  b = [real(Bc); imag(Bc)];
  
  Escale = zeros(1, np + 1);
  for i = 1:np+1
    Escale(i) = norm(A(:,i), 2);
    A(:,i) = A(:,i) ./ Escale(i);
  endfor
  
  # Solve the least-squares problem
  x = A \ b;  # A is 2*N-by-(Np+1) & b is 2*N-by-1, so x is Np+1-by-1
  x = x ./ Escale.';
  
  # Redefine C & D - store D for output
  C = x(1:np,1).';
  SER.D = x(np+1);
  
  # ----------------------------------------------------------------------------
  # Make C complex-valued
  i = 1;
  Cc = zeros(1, np);  # 1-by-Np
  while i <= np
    if cindex(i) == 1
      r1 = C(i);
      r2 = C(i+1);
      Cc(i) = r1 + 1i * r2;
      Cc(i+1) = r1 - 1i * r2;
      i = i + 2;
    else
      i = i + 1;
    endif
  endwhile
  
  # ----------------------------------------------------------------------------
  # Compute the fit, and then determine the error (residual)
  Dk = zeros(n, np);  # Complex-Valued
  for i = 1:np
    Dk(:,i) = 1 ./ (s - poles(i));
  endfor
  fit = Dk * Cc.' + SER.D;    # N-by-1 - Complex-Valued
  dif = fit - frf;            # N-by-1 - Complex-Valued
  
  # ----------------------------------------------------------------------------
  # Convert into a real-valued state-space model
  A = zeros(np, np);  # Np-by-Np
  B = ones(np, 1);    # Np-by-1
  
  j = 0;
  for i = 1:np
    j = j + 1;
    if cindex(i) == 1
      a1 = real(poles(j));
      a2 = imag(poles(j));
      c1 = real(Cc(j));
      c2 = imag(Cc(j));
      b1 = 2 * real(B(j));
      b2 = -2 * imag(B(j));
      Ablock = [a1  a2;-a2  a1];
      
      A(j:j+1,j:j+1) = Ablock;
      C(j) = c1;
      C(j+1) = c2;
      B(j) = b1;
      B(j+1) = b2;
    endif
  endfor
  
  # Output
  SER.A = A;
  SER.B = B;
  SER.C = C;
  
endfunction

################################################################################
function p = estimate_poles(omega, order)
  # Creates an initial estimate of poles
  npts = length(omega);
  beta = linspace(omega(1), omega(npts), order / 2);
  poles = zeros(1, order);
  j = 1;
  for i = 1:length(beta)
    alf = -1e-2 * beta(i);
    arg = [alf - 1i * beta(i), alf + 1i * beta(i)];
    poles(j:j+1) = arg;
    j = j + 2;
  endfor
  p = poles;
endfunction

################################################################################
function cindex = find_complex(x)
  # Finds complex values and conjugates.  The labelling scheme is as follows:
  # 0: Real value
  # 1: Complex value
  # 2: Complex conjugate of the previous value in the array.
  tol = sqrt(eps);
  n = length(x);
  cindex = zeros(1, n);
  for i = 1:n
    if imag(x(i)) ~= 0
      if i == 1
        cindex(i) = 1;
      else
        if abs(imag(x(i)) + imag(x(i-1))) <= tol
          cindex(i) = 2;
        else
          cindex(i) = 1;
        endif
      endif
    endif
  endfor
endfunction
