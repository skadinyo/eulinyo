include("Math.jl")

function problem_1(lim)
  res = 0
  for i in [1:(lim-1);]
    if (0==(rem(i,3)))||(0==(rem(i,5)))
      res = res + i
    end
  end
  return res
end

function problem_2(lim)
  res = 0
  a = 1
  b = 2
  while a < lim
    if iseven(a)
      res = res + a
    end
    c = a + b
    a = b
    b = c
  end
  return res
end

function problem_3(x)
  i = 2
  temp = x
  max_i = 0
  while temp > 1
    if 0==rem(temp, i)
      temp = div(temp, i)
      max_i = i
    else
      i = next_prime(i)
    end
  end
  return max_i
end
