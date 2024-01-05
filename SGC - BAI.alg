/*Projecto - Grupo nº 7
 * Elementos:
 * 			- Barquisse Fulai David, nº 221075
 * 			- Albano Patrick, nº 228778
 * 			- Sílvio Correia, nº 223161
 * 	Cadeira: Programação I
 * 	Docente: Lukau Garcia
 * 	
 * 	Objetivo: De formas a melhorar a gestão e controlo de seus clientes e seus
respectivos dinheiros, foram convidados os alunos do curso de
Informática de Gestão Financeira do ISAF a desenvolver um sistema
que vai ajudar o BAI nas suas actividades
*/


registo PESSOA
	literal nome
	literal bi
	inteiro telefone
	inteiro codigo
fimregisto

registo CONTA
	inteiro num_conta
	real saldo
	inteiro codigo_cliente
	inteiro tipo_conta
	literal estado
fimregisto

//Elementos para a criação do vector
constante inteiro TAMANHO<- 1000
inteiro contlinha<- 0
inteiro TotalCliente<-0
inteiro TotalContas<-0

//Criação do Ficheiro
literal caminho <- "C:\\Users\\silvi_5cnyxi8\\OneDrive\\Documents\\Documentos ISAF\\Aulas - IGF 2º ano M1\\1º Semestre\\Programação I\\Projecto Programação\\Projeto.txt"

funcao logico Criarficheiro()
fficheiro f

		logico ficheiroExistente <- fExiste(caminho)

		se nao ficheiroExistente entao
			se nao fcriar (caminho) entao 
				mostre "Não foi possivel criar o Ficheiro"
				fimalgoritmo
			fimse
		fimse

		fAbrir (f, caminho, FALSO)

		enquanto ftemlinha(f) faca 
			fLerlinha(f)
			contlinha <- contlinha + 1
		fimenquanto
		fFechar(f)
	retorne verdadeiro
fimfuncao

//Vectores
PESSOA clientes[TAMANHO + contlinha]
CONTA contas[TAMANHO+contlinha]

///Funções auxiliares
funcao inteiro identificar_cliente (inteiro codigo_pessoa)
	inteiro espaco
	para espaco de 0 ate TotalCliente-1
		PESSOA p<- clientes[espaco]
		se p.codigo=codigo_pessoa entao
			retorne espaco
		fimse
	fimpara

	retorne -1
fimfuncao

funcao inteiro identificar_conta(inteiro codigo_cliente)
	inteiro espaco
	para espaco de 0 ate TotalContas-1
		CONTA cnt <- contas[espaco]
		se cnt.codigo_cliente = codigo_cliente entao
			retorne espaco
		fimse
	fimpara
	retorne -1
fimfuncao

funcao inteiro procurar_conta(inteiro num_conta)
	inteiro espaco
	para espaco de 0 ate TotalContas-1
		CONTA cnt <-contas[espaco]
		se cnt.num_conta = num_conta entao
			retorne espaco
		fimse
	fimpara

	retorne -1
fimfuncao

procedimento continuar()
    literal qualquertecla
    mostre "\nPressione em qualquer tecla para continuar."
    leia qualquertecla
fimprocedimento

//Procedimento responsável por disponibilizar as informações do Ficheiro Físico.
procedimento restaurar_backup_isaf()
	fficheiro ficheiro

	fAbrir(ficheiro, caminho, falso)
	fLerLinha(ficheiro)
	
	enquanto fTemLinha(ficheiro) faca
		literal linha<- fLerLinha(ficheiro)
		literal linha_separar[]<- strRepartir(linha, ";")

		PESSOA p
		p.codigo<-strValorInt(linha_separar[0])
		p.nome<-linha_separar[1]
		p.bi<-linha_separar[2]
		p.telefone<-strValorInt(linha_separar[3])

		clientes[TotalCliente]<-p
		TotalCliente<- TotalCliente+1
		
		se aTamanho(linha_separar, 0) = 4 entao
			continue
		fimse
		
		CONTA cnt
		cnt.num_conta<- strValorInt(linha_separar[4])
		cnt.saldo<- strValorReal(strTrocar(linha_separar[5], ",", ""))
		cnt.codigo_cliente<- strValorInt(linha_separar[6])
		cnt.tipo_conta<- strValorInt(linha_separar[7])
		cnt.estado<- linha_separar[8]

		contas[TotalContas]<- cnt
		TotalContas<- TotalContas+1
	fimenquanto
	fFechar(ficheiro)
fimprocedimento

//Função Cadastrar Clientes
funcao logico cadastro_de_cliente(PESSOA p)
	
	clientes[TotalCliente]<- p
	TotalCliente<-TotalCLiente+1
	retorne verdadeiro
	
fimfuncao

//Função para Listar os Clientes Cadastrados
procedimento listar_clientes()
	inteiro contador, espaco
	para contador de 0 ate TotalCliente-1
		PESSOA p<-clientes[contador]
		escreva "=====================================\n"
		escreva "Nome: ",p.nome,"\n"
		escreva "BI/NIF: ",p.bi,"\n"
		escreva "Telefone: ",p.telefone,"\n"
		escreva "Código: ", p.codigo,"\n"

		espaco<-identificar_conta(p.codigo)
		
		se espaco <>-1 entao
			CONTA cnt<-contas[espaco]
			
			escreva "\nNúmero da Conta: ",cnt.num_conta
			escreva "\nSaldo:      AKZ ",cnt.saldo
			escreva "\nCódigo do Cliente: ",cnt.codigo_cliente
			escreva "\nTipo de Conta: ",cnt.tipo_conta
			escreva "\nEstado da conta: ",cnt.estado
			escreva "\n=====================================\n"

		senao 
			escreva "O cliente cadastrado não possui uma conta."
			escreva "\n=====================================\n"
		fimse
	fimpara
fimprocedimento 

//Função responsãvel por abrir as contas do clientes cadastrados.
funcao logico abertura_de_conta(PESSOA p, inteiro tipo_de_conta)
	CONTA cnt
	cnt.num_conta<- TotalContas+1
	cnt.saldo<-0
	cnt.codigo_cliente<-p.codigo
	cnt.tipo_conta<-tipo_de_conta
	cnt.estado<-"Desativado"

	escreva "============================="
	escreva "\nNúmero da Conta: ",cnt.num_conta
	escreva "\nSaldo:      AKZ ",cnt.saldo
	escreva "\nCódigo do Cliente: ",cnt.codigo_cliente
	escreva "\nTipo de Conta: ",cnt.tipo_conta
	escreva "\nEstado da conta: ",cnt.estado
	escreva "\n=============================\n"
	
	contas[TotalContas]<- cnt
	TotalContas<- TotalContas+1
	retorne verdadeiro
fimfuncao

//Função para depositar dinheiro
funcao logico depositar_dinheiro(inteiro num_conta, real valor)
		inteiro espaco<-procurar_conta(num_conta)
		
		
		se espaco <>-1 entao
			contas[espaco].saldo<-contas[espaco].saldo+valor
			contas[espaco].estado<-"Ativado"
			retorne verdadeiro
		senao
			escreva "Impossível realizar operação"
		fimse

	retorne falso
fimfuncao

//Função para levantar dinheiro
funcao logico levantar_dinheiro (inteiro num_conta, real valor)
	inteiro espaco<-procurar_conta(num_conta)
	
	se espaco<>-1 entao
		se contas[espaco].saldo >= valor entao
			contas[espaco].saldo<-contas[espaco].saldo-valor
			retorne verdadeiro
		senao
			escreva "Impossível realizar operação"
		fimse
	fimse
	retorne falso
fimfuncao

//Função para consultar o saldo das contas
funcao real consultar_saldo(inteiro num_conta)
	inteiro espaco<-procurar_conta(num_conta)	
	se espaco <> -1 entao
		retorne contas[espaco].saldo
	fimse
	
	retorne -1
fimfuncao

//Função para transferir o dinheiro de uma conta para outra
funcao logico transferir_dinheiro(inteiro num_conta_enviar, inteiro num_conta_receber, real valor)
	
	inteiro espacoEnviar<-procurar_conta(num_conta_enviar)
	inteiro espacoReceber<-procurar_conta(num_conta_receber)
	
	se espacoEnviar<>-1 E espacoReceber<>-1 entao
		se contas[espacoEnviar].saldo >= valor e contas[espacoEnviar].estado<>"Desativado" e contas[espacoReceber].estado <> "Desativado" entao
			contas[espacoEnviar].saldo<-contas[espacoEnviar].saldo-valor

			contas[espacoReceber].saldo<-contas[espacoReceber].saldo+valor
			retorne verdadeiro
		senao
			
			se contas[espacoEnviar].estado="Desativado" entao
				escreva "\nA conta a transferir está indisponível"
			fimse
			se contas[espacoReceber].estado="Desativado" entao
				escreva"\nA conta a receber está indisponível"
			fimse

			se contas[espacoEnviar].estado="Ativado" e contas[espacoEnviar].saldo<valor  entao
				escreva "\nO saldo da conta ", contas[espacoEnviar].num_conta, " é insuficiente."
			fimse
			escreva "\nImpossível realizar operação!"
		fimse
	fimse
	retorne falso
fimfuncao

//Função para a atualização de dados
funcao logico atualizar_dados(PESSOA p, inteiro codigo_pessoa)
	CONTA cnt
	inteiro espaco<-identificar_cliente(codigo_pessoa)
	se espaco = -1 entao
		retorne falso
	fimse
	clientes[espaco].nome<-p.nome
	clientes[espaco].bi<-p.bi
	clientes[espaco].telefone<-p.telefone
	retorne verdadeiro
fimfuncao


//Função responsável por salvar as informações no ficheiro físico
procedimento backup_isaf()
	fficheiro ficheiro
	fAbrir(ficheiro, caminho, verdadeiro)
	inteiro contador
	fEscrever(ficheiro,"Código; Nome; BI/NIF; Telefone; Número da Conta; Saldo; Código do Cliente; Tipo de Conta; Estado\n" )
	para contador de 0 ate TotalCliente-1
		PESSOA p<- clientes[contador]
		literal linha<- strIntParaStr(p.codigo)+";"+p.nome+";"+p.bi+";"+strIntParaStr(p.telefone)
	
		inteiro espaco<-identificar_conta(p.codigo)
		se espaco<>-1 entao
			CONTA cnt<-contas[espaco]
			linha<- linha + ";" + strIntParaStr(cnt.num_conta) + ";" + strRealParaStr(cnt.saldo) + ";" + strIntParaStr(cnt.codigo_cliente) + ";" + strIntParaStr(cnt.tipo_conta) + ";" + cnt.estado 
		fimse
		fEscrever(ficheiro, linha+"\n")
	fimpara
	fFechar(ficheiro)
fimprocedimento

procedimento menu()
		escreva "=========SISTEMA DE GESTÃO DE CLIENTES BAI=========", "\n"
		// Exibir menu de opções
        escreva "======== Menu ========\n"
        escreva "1: Cadastrar Cliente\n"
        escreva "2: Listar Clientes\n"
        escreva "3: Abertura de Conta\n"
        escreva "4: Depositar Dinheiro\n"
        escreva "5: Levantar Dinheiro\n"
        escreva "6: Consultar Saldo\n"
        escreva "7: Transferir Dinheiro\n"
        escreva "8: Actualizar Dados do Cliente\n"
        escreva "9: Terminar\n"
        
fimprocedimento

inicio
	Criarficheiro()
	restaurar_backup_isaf()
repita
	limpatela
	menu()
	logico resposta
	inteiro opcao
	escreva "Escolha uma opção: "
	leia opcao

	
	escolha opcao
	caso 1:
		limpatela

		mostre "===================Cadastro de Clientes=============\n\n"
		PESSOA p
		escreva "Insira o nome: "
		leia p.nome
		escreva "Insira o número do BI/NIF: "
		leia p.bi
		escreva "Insira o numero do telefone: "
		leia p.telefone
		
		inteiro space
		para space de 0 ate TotalCliente-1 
		p.codigo<-clientes[space].codigo+1
		fimpara
		
		resposta<- cadastro_de_cliente(p)
		tpause(1000)
		limpatela

		escreva "Nome: ", p.nome,"\n"
		escreva "Nº de BI/NIF: ",p.bi,"\n","Nº de telefone: ",p.telefone,"\n"
		escreva "O código é: ", p.codigo,"\n"

		se resposta = verdadeiro entao
			escreva "\nOperação Realizada com Sucesso\n"
		senao
			escreva "\nTente novamente"
		fimse

		continuar()
	caso 2:
		limpatela
		listar_clientes()
		continuar()
		tpause(2000)
	caso 3:
		limpatela

		escreva "==================== Abertura de Conta ==================\n\n"
		inteiro codigo, tipo_conta, espaco
		escreva "Digite o código pessoal: "
		leia codigo
		
		espaco<-identificar_cliente(codigo)
		se espaco <>-1 entao
		
			escreva "Que tipo de conta deseja abrir?\n"
			escreva "0- Conta Particular \n1-Conta Empresarial\n"
			escreva "Conta: "
			leia tipo_conta
			limpatela
			escreva "==================== Abertura de Conta ==================\n\n"
			resposta<-abertura_de_conta(clientes[espaco], tipo_conta)
		senao
			resposta<-falso
		fimse

		se resposta = verdadeiro entao
			escreva "\nOperação Realizada com Sucesso\n"
		senao
			escreva "\nTente novamente."
		fimse
		
		continuar()
		tpause(1000)
	caso 4:
		limpatela
		inteiro codigo
		real valor
		escreva"======================Depositar Dinheiro========================\n\n"

		escreva "Insira o seu número de conta: "
		leia codigo
		
		escreva "Digite o valor a depositar: "
		leia valor
		
		resposta<-depositar_dinheiro(codigo, valor)
		
		se resposta = verdadeiro entao		
			escreva "Depositou: ", valor, "\n"
		senao
			//limpatela
			escreva "\nNumero errado! Tente novamente\n"					
			//menu()
		fimse
		continuar()
		tpause(1000)
			
	caso 5:
		limpatela
		inteiro codigo
		real valor
		escreva"======================Levantar Dinheiro========================\n"

		escreva "Insira o número da conta: "
		leia codigo

		escreva "Digite o valor a levantar: "
		leia valor

		resposta<- levantar_dinheiro(codigo, valor)
		se resposta = verdadeiro entao
			escreva "Levantou: ", valor, "\n"
		senao
			escreva "\nTente novamente\n"
		fimse
		continuar()
		tpause(1000)
	caso 6:
		limpatela
		inteiro codigo
		real retorno
		escreva"======================Consultar Saldo========================\n"

		escreva "Insira o numero da conta: "
		leia codigo

		retorno<-consultar_saldo(codigo)

		se retorno <>-1 entao
			escreva "\nSALDO:     AKZ ", retorno
			resposta <- verdadeiro
			
			se retorno = -1 entao
				escreva "\nTente novamente!"
				resposta <- falso
			fimse
		fimse
		continuar()
		tpause(1000)
		
	caso 7: 
		limpatela
		escreva "=================Transferir Dinheiro=================\n"
		inteiro codigoEnviar, codigoReceber
		real valor

		escreva "Insira o número de conta a enviar: "
		leia codigoEnviar

		escreva "Insira o número de conta a receber: "
		leia codigoReceber

		escreva "Insira o valor a transferir: "
		leia valor
		
		resposta<- transferir_dinheiro(codigoEnviar, codigoReceber, valor)
		se resposta = verdadeiro entao
			escreva "Transferiu  AKZ ", valor, " da conta ",codigoEnviar," para a conta ",codigoReceber
		senao
			escreva "\nTente novamente!\n"
		fimse
		tpause(1000)
		continuar()

	caso 8:
		limpatela
		mostre "\n===================Atualização de Dados=================\n"
			inteiro codigo
			PESSOA p
			CONTA cnt
			escreva "Digite o código pessoal: "
			leia codigo
			inteiro espaco
			espaco<-identificar_cliente(codigo)
			se espaco <>-1 entao
				escreva "Digite o nome: "
				leia p.nome
				escreva "Digite o número do BI/NIF: "
				leia p.bi
				escreva "Digite o número de telefone: "
				leia p.telefone
			senao
				escreva "Insira o codigo correto\n!"
			fimse
			resposta<- atualizar_dados(p, codigo)

			se resposta = verdadeiro entao
				escreva "\nOperação Realizada com Sucesso\n"
			senao
				escreva "Tente novamente"
			fimse
			tpause(1000)
			continuar()
	caso 9:
			limpatela
			escreva "==FIM DE OPERAÇÕES=="
			tpause(1000)
			limpatela
	defeito:
		
	fimescolha

	se resposta = verdadeiro entao
		backup_isaf()
		tpause(1000)
		limpatela
	fimse
ate opcao=9
fimalgoritmo