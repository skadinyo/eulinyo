function count_number(x::Int64)
  temp = x
  res = 1
  while temp > 10
    temp = div(temp,10)
    res += 1
  end
  return res
end

function next_prime(x)
  if iseven(x)
    i=x+1
  else
    i=x+2
  end
  while ~(isprime(i))
    i=i+2
  end
  return i
end
