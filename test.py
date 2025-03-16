import fastalgebra
orig= "2*("*70+"-8)"*70
b = fastalgebra.Expression("2*("*70+"-8)"*70)
print(b.stringify(), "\n", orig)

