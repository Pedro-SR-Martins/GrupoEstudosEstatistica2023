* Encoding: UTF-8.

* esse script faz com que os dados passem do formato wide para formato longo.


* no exemplo, temos duas variaveis de medidas repetidas, extroversao e depressao medidas 4 vezes.
*criamos as variaveis novas a partir das varias medicoes.
*a quantidade de vezes medidas eh transferida para a variavel time(4), se tivessemos outra quantidade de medidas repetidas, deveriamos trocar o numero ao lado.
*as outras variaveis do banco de dados (id e sexo) sao mantidas como fatores fixos (medidos apenas uma vez no tempo) :D.
VARSTOCASES
  /MAKE depression FROM dep_t1 dep_t2 dep_t3 dep_t4 
  /MAKE extraversion FROM ext_t1 ext_t2 ext_t3 ext_t4
  /INDEX=time(4) 
  /KEEP=id sexo 
  /NULL=KEEP.
