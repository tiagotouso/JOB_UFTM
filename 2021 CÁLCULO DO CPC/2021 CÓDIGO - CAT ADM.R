dir()


dados = read.csv2("Portal_CPC_Edição2019_atualizado.csv")
colnames(dados)

ufs <- unique(dados[,c("Código.da.IES", "Categoria.Administrativa")])
View(ufs)

write.csv2(ufs, file = '2019 CÓDIGO - CAT ADM.csv')
