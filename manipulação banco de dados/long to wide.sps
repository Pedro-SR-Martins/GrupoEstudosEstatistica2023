* Encoding: UTF-8.
* esse script faz com que os dados passem do formato longo para formato wide.

*IMPORTANTE: sua variavel de identificacao dos sujeitos deve ter o nome id (mas voce pode editar o codigo, claro). 
* sua variavel de marcacao das medidas repetidas (ou dos tempos) esta nomeada como time.
SORT CASES BY id time.
CASESTOVARS
  /ID=id
  /INDEX=time
  /GROUPBY=VARIABLE.
