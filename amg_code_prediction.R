require(xtable)
#Importação dos dados

#Deu certo
#Dados Iris
x_treino=read.csv(file.choose())
x_teste=read.csv(file.choose())
#colnames(iris)
y_treino=x_treino$Sepal.Length
y_teste=x_teste$Sepal.Length 
x_treino=x_treino[,c("Sepal.Width","Petal.Length","Petal.Width")]
x_teste=x_teste[,c("Sepal.Width","Petal.Length","Petal.Width")]

#Deu certo
#Dados mpg
x_treino=read.csv(file.choose())
x_teste=read.csv(file.choose())
y_treino=x_treino$acceleration
y_teste=x_teste$acceleration

head(x_treino)
x_treino=x_treino[,-6]
x_teste=x_teste[,-6]

#Dá certo em algumas iterações, outras não
#Dados Slump test
x_treino=read.csv(file.choose())
x_teste=read.csv(file.choose())
y_treino=x_treino$Compressive.Strength..28.day..Mpa.
y_teste=x_teste$Compressive.Strength..28.day..Mpa.

head(x_treino)
x_treino=x_treino[,-10]
x_teste=x_teste[,-10]

#Deu certo para poucas iterações, banco de dados grande.
#Dados parkinson
x_treino=read.csv(file.choose())
x_teste=read.csv(file.choose())
y_treino=x_treino$motor_UPDRS
y_teste=x_teste$motor_UPDRS

head(x_treino)
x_treino=x_treino[,-5]
x_teste=x_teste[,-5]

#Em algumas iterações o algoritmo não converge, solução: 
#fazer várias vezes até a quantidade necessária
#Dados preços de casas
x_treino=read.csv(file.choose())
x_teste=read.csv(file.choose())
y_treino=x_treino$Y.house.price.of.unit.area
y_teste=x_teste$Y.house.price.of.unit.area

head(x_treino)
x_treino=x_treino[,-6]
x_teste=x_teste[,-6]

#Deu certo
#dados de toxicidade aquática aguda em relação 
#às promelas de Pimephales de peixes (peixinho de fathead) 
x_treino=read.csv(file.choose(),sep=",")
x_teste=read.csv(file.choose(),sep=",")
y_treino=x_treino$V3
y_teste=x_teste$V3

head(x_treino)
x_treino=x_treino[,-3]
x_teste=x_teste[,-3]

#deu certo
#dados da análise de energia de construções simuladas
x_treino=read.csv(file.choose(),sep=",")
x_teste=read.csv(file.choose(),sep=",")
y_treino=x_treino$Y2
y_teste=x_teste$Y2

head(x_treino)
x_treino=x_treino[,-9]
x_teste=x_teste[,-9]


#Pré-processamento
sapply(x_treino,class)
sapply(x_teste,class)
for(i in 1:ncol(x_treino))
{
  if(sapply(x_treino,class)[i]=="integer")
  {
    x_treino[,i]=as.numeric(x_treino[,i])
    x_teste[,i]=as.numeric(x_teste[,i])
  }
}

sapply(x_treino,class)
sapply(x_teste,class)


#hiperparâmetros finais

num_minimo_terminal=5
num_minimo_interno=20
prof_max=2
porc_min=0.8419415
tipo_poda=2
porc_modelos=0.797742
num_minimo_modelos=3
num_min_obs_test_var=5
porc_validacao=0.273396


{
      #Fazendo validação se necessário
      if(tipo_poda==1)
      {
        amostra2=sample(1:nrow(x_treino),ceiling(nrow(x_treino)*porc_validacao),replace=F)
        x_validacao =x_treino[amostra2,]
        y_validacao =y_treino[amostra2]
        x_treino =x_treino[-amostra2,]
        y_treino =y_treino[-amostra2]
      }
      
      #vetor_ind_obs=list()
      #Função que faz todas as medias consecutivas da variavel explicativa numerica
      retorna_medias_var_numerica=function(x)
      {
        #Seleciona os únicos e ordena
        x=sort(unique(x))
        #Vetor de médias
        medias=c()
        for(i in 1:length(x))
        {
          if(i!=length(x))
          {
            medias=append(medias,(x[i]+x[i+1])/2)
          }
        }
        return(medias)
      }
      
      #Funcao que faz o melhor separador para variavel explicativa numerica,
      #ela será usada em cada nó da árvore, em quanto outra função chamará ela em todos os nós.
      #Observação: nossa algoritmo tem que existir pelo menos uma variável numérica,
      #ou temos que pensar melhor nessa função.
      
      separador_subconjuntos=function(x,y){
        separador=sd(y)
        #Valor separador
        separador2=0
        #Indice da coluna do melhor separador
        coluna_var_separadora=3
        #Contador
        contador=0
        #Vetor de tipos das variáveis
        #a=sapply(x,class)
        #Posição de alguma coluna numérica se o subconjunto não tiver separador "bom",
        #ou seja, o caso em que nenhum separador melhora o ganho em desvio-padrão. 
        #a=which(a=="numeric")[1]
        
        for(i in 1:ncol(x))
        {
          #if(class(x[,i])=="numeric")
          #{
          #Chamando a função que dá as médias ordenadas
          medias_separadoras=retorna_medias_var_numerica(x[,i])
          #print(medias_separadoras)
          for(j in 1:length(medias_separadoras))
          {
            #Posicoes que atendem a condicao
            posicoes_obs_atendidas=which(x[,i]<=medias_separadoras[j])
            #Posicoes que não atendem a condicao
            posicoes_obs_nao_atendidas=setdiff(1:length(x[,i]),posicoes_obs_atendidas)
            #Calculando todos os desvios-padroes ponderados e verificando se é melhor 
            #trocar para esse separador
            if(length(posicoes_obs_atendidas)>=num_min_obs_test_var & 
               length(posicoes_obs_nao_atendidas)>=num_min_obs_test_var)
            {
              desvio_padrao_ponderado=((length(posicoes_obs_atendidas)/length(x[,i]))*
                                         sd(y[posicoes_obs_atendidas])+(length(posicoes_obs_nao_atendidas)/length(x[,i]))*
                                         sd(y[posicoes_obs_nao_atendidas]))
              
              if(desvio_padrao_ponderado < separador)
              {
                contador=contador+1
                separador=desvio_padrao_ponderado
                separador2=medias_separadoras[j]
                coluna_var_separadora=i
              }
            }
          }
          #Verificação se não existe redução do desvio-padrão da variável resposta
          if(contador==0)
          {
            #Pondo um valor do separador extremamente específico para que identifiquemos
            #partições inutéis.
            separador2=123456789
            #Posição da primeira variável numérica encontrada, usada também para
            #identificar
            coluna_var_separadora=1
          }
          #}
        }
        #Retorna o valor separador e o nome da variável que é do melhor separador.
        return(list(separador2,colnames(x)[coluna_var_separadora]))
      }
      
      
      #Funcao de construcao da arvore, retorna uma lista com todas as observacoes em cada
      #nó da árvore
      #Chama as funções separador_subconjuntos, que chama a função retorna_medias_var_numerica
      #para nó, e assim construir a primeira versão da árvore completa, com cada observação
      #em seu respectivo nó.
      
      constroi_arvore=function(x,y)
      {
        #Listas para receber o separador e variável separadora
        lista_temporaria2=c()
        lista_temporaria3=c()
        #Indice de todas as observações
        ind_obs=list(1:length(y))
        #Realizando as partições com a profundidade máxima
        for(i in 1:(2^(prof_max)-1))
        {
          #Contando com o fato de que existe pelo menos um separador
          if(i>=2)
          {
            if(length(ind_obs[[floor(i/2)]])>1 && lista_temporaria2[1]!=123456789 )
            {
              lista_temporaria=separador_subconjuntos(x[ind_obs[[i]],],y[ind_obs[[i]]])
              #print(lista_temporaria)
              lista_temporaria2[i]=lista_temporaria[[1]]
              lista_temporaria3[i]=lista_temporaria[[2]]
              #Observações do filho esquerdo
              pos_lado_e=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                             <=lista_temporaria[[1]])])
              #Observações do filho direito
              pos_lado_d=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                             >lista_temporaria[[1]])])
              #Condições dos hiperparâmetros para filhos não-folhas
              if((2*i<=(2^(prof_max)-1)) & length(pos_lado_d)>=num_minimo_interno &
                 length(pos_lado_e)>=num_minimo_interno & sd(y[ind_obs[[i]]])>=sd(y)*porc_min
                 & length(pos_lado_d)>0 & length(pos_lado_e)>0)
              { 
                ind_obs[[2*i]]=pos_lado_e
                ind_obs[[2*i+1]]=pos_lado_d
              }
              else
              {
                #Condições dos hiperparâmetros para filhos folhas
                if((2*i>(2^(prof_max)-1)) & length(pos_lado_d)>=num_minimo_terminal &
                   length(pos_lado_e)>=num_minimo_terminal & sd(y[ind_obs[[i]]])>=
                   sd(y)*porc_min & length(pos_lado_d)>0 & length(pos_lado_e)>0)
                {
                  ind_obs[[2*i]]=pos_lado_e
                  ind_obs[[2*i+1]]=pos_lado_d
                }
                else
                {
                  ind_obs[[2*i]]=0
                  ind_obs[[2*i+1]]=0   
                }        
              }
            }
            else
            {
              ind_obs[[2*i]]=0
              ind_obs[[2*i+1]]=0 
            }
          }
          else{
            lista_temporaria=separador_subconjuntos(x[ind_obs[[i]],],y[ind_obs[[i]]])
            #print(lista_temporaria)
            lista_temporaria2[i]=lista_temporaria[[1]]
            lista_temporaria3[i]=lista_temporaria[[2]]
            if(lista_temporaria2[1]==123456789)
            {
              ind_obs[[2]]=0
              ind_obs[[3]]=0
            }
            else
            {
              pos_lado_e=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                             <=lista_temporaria[[1]])])
              pos_lado_d=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                             >lista_temporaria[[1]])])
              ind_obs[[2*i]]=pos_lado_e
              ind_obs[[2*i+1]]=pos_lado_d
            }
            
          }
        }
        # print(ind_obs)
        return(list(ind_obs,lista_temporaria2,lista_temporaria3))  
      }
      
      
      arvore1=constroi_arvore(x_treino,y_treino)
      arvore=arvore1[[1]];arvore
      data_frame_sep=cbind.data.frame(arvore1[[2]],arvore1[[3]])
      colnames(data_frame_sep)=c("Valores Separadores","Variáveis Separadoras")
      
      dados=cbind.data.frame(x_treino,y_treino)
      
      coef_modelo=list()
      modelos_links=c()
      modelos_dist=c()
      indices_modelos_retirados=c()
      erro_menor=rep(-1,length(arvore))
      melhores_modelos=list()
      indices_modelos=1:9
      
      for(k in 1:length(arvore))
      {
        if(arvore[[k]]!=0)
        {
          #k=1, remover
          dados2=dados[arvore[[k]],]
          
          modelos=list(7)
          
          erros_temporarios=c()
          indices_modelos=setdiff(indices_modelos,indices_modelos_retirados)
          #print(indices_modelos)
          for(p in indices_modelos)
          {
            if(p==1)
            {
              modelo1=glm(y_treino~.,data=dados2,family=gaussian(link="identity"))
              modelos[[1]]=modelo1
            }else if(p==2)
            {
              modelo3=glm(y_treino~.,data=dados2,family=gaussian(link="inverse"))
              modelos[[2]]=modelo3
            }else if(p==3)
            {
              modelo4=glm(y_treino~.,data=dados2,family=gaussian(link="log"))
              modelos[[3]]=modelo4
            }else if(p==4)
            {
              modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="identity"))
              modelos[[4]]=modelo2
            }else if(p==5)
            {
              modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="inverse"))
              modelos[[5]]=modelo2
            }else if(p==6)
            {
              modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="log"))
              modelos[[6]]=modelo2
            }else if(p==7)
            {
              modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="identity"))
              modelos[[7]]=modelo2
            }else if(p==8)
            {
              modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="inverse"))
              modelos[[8]]=modelo2
            }else{
              modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="log"))
              modelos[[9]]=modelo2
            }
          }
          #print(length(indices_modelos))
          erro_menor[k]=1000000
          #9 porque é o tamanho total de modelos
          for(i in 1:9)
          {
            if(is.element(i,indices_modelos))
            {
              #print(i)
              erro=mean(abs(y_treino[arvore[[k]]]-modelos[[i]]$fitted.values))
              erros_temporarios[i]=erro
              if(erro<erro_menor[k])
              {
                melhores_modelos[[k]]=modelos[[i]]
                coef_modelo[[k]]=modelos[[i]]$coefficients
                modelos_links[k]=modelos[[i]]$family$link
                modelos_dist[k]=modelos[[i]]$family$family
                erro_menor[k]=erro
              }
            }
            else
            {
              erros_temporarios[i]=NA 
            }
          }
          ind=length(which(is.na(erros_temporarios)==TRUE))
          erros_ordenados=order(erros_temporarios)
          (indices_modelos_retirados=erros_ordenados[(ceiling((length(
            erros_ordenados)-ind)*porc_modelos)):(length(erros_ordenados)-ind)])
          melhores_modelos[[k]]=step(melhores_modelos[[k]],direction = "backward")
          coef_modelo[[k]]=melhores_modelos[[k]]$coefficients
          
        }
        else
        {
          melhores_modelos[[k]]=0
          coef_modelo[[k]]=0
          modelos_links[k]="nao_existe"
          modelos_dist[k]="nao_existe"
        }
        if(length(setdiff(indices_modelos,indices_modelos_retirados))<num_minimo_modelos)
        {
          indices_modelos_retirados=c()
        }
      }
      
      predict=function(x,i=1)
      {
        if(2*i>=(2^prof_max) && (arvore[[i]])!=0 && (arvore[[2*i]])!=0
           && (arvore[[2*i+1]])!=0)
        {
          if(x[which(colnames(x)==data_frame_sep[i,2])]<=data_frame_sep[i,1])
          {
            return(2*i)
          }
          else
          {
            return(2*i+1)
          } 
        }else if((arvore[[1]])!=0 && (arvore[[2]])==0
                 && (arvore[[3]])==0)
        {
          print("Só tem o modelo da raiz!")
          return(1)
          
        }else if(arvore[[2*i]]!=0 && arvore[[4*i]]==0
                 && (arvore[[2*(2*i+1)]])!=0 && 2*i<(2^prof_max))
        {
          if(x[which(colnames(x)==data_frame_sep[i,2])]<=data_frame_sep[i,1])
          {
            return(2*i)
          }
          else
          {
            return(predict(x,2*i+1))
          }
        }else if((arvore[[i]])!=0 && (arvore[[2*i+1]])!=0 && 
                 (arvore[[2*(2*i+1)]])==0 && (arvore[[4*i]])!=0 
                 && 2*i<(2^prof_max))
        {
          if(x[which(colnames(x)==data_frame_sep[i,2])]>data_frame_sep[i,1])
          {
            return(2*i+1)
          }
          else
          {
            return(predict(x,2*i))
          }
        }else if(2*i<(2^prof_max) && (arvore[[4*i]])==0 &&
                 (arvore[[2*(2*i+1)]])==0)
        {
          if(x[which(colnames(x)==data_frame_sep[i,2])]<=data_frame_sep[i,1])
          {
            return(2*i)
          }
          else
          {
            return(2*i+1)
          }
          
        }else if((arvore[[i]])!=0 && (arvore[[2*(2*i+1)]])!=0 &&
                 (arvore[[4*i]])!=0 && 2*i<(2^prof_max) )
        {
          if(x[which(colnames(x)==data_frame_sep[i,2])]<=data_frame_sep[i,1])
          {
            return(predict(x,2*i))
          }
          else
          {
            return(predict(x,2*i+1))
          }
        }else
        {
          print("Aconteceu algo estranho!")
          return(-1)
        }
      }
      
      #Função que prediz o valor de uma única observação
      predict2=function(x)
      {
        return(predict.glm(melhores_modelos[[predict(x)]],x,type="response"))
      }
      
      #Função que prediz os valores de várias observações
      predict3=function(x)
      {
        valores_preditos=c()
        for(i in 1:nrow(x))
        {
          valores_preditos=append(valores_preditos,predict2(x[i,]))
        }
        return(valores_preditos)
      }
      
      erro_medio_absoluto=function(y_real,y_pred)
      {
        return(mean(abs(y_real-y_pred)))
      }
      
      #Função que fiz o subconjunto(folha) final do conjunto de validação
      if(tipo_poda==1)
      {
        
        subconjunto_folha=c()
        for(i in 1:nrow(x_validacao))
        {
          subconjunto_folha=append(subconjunto_folha,predict(x_validacao[i,]))
        }
      }
      
      #Para quando só se 1 modelos na árvore
      if(T)
      {
        if(data_frame_sep[1,1]==123456789)
        {
          print("Nao tem poda1!")
        }
        else
        {
          #Função dos últimos índices de cada nível da árvore
          niveis_prof=c(1)
          for(j in 1:(prof_max+1))
          {
            niveis_prof=append(niveis_prof,(2^(j+1))-1)
          }
          
          
          if(tipo_poda==1)
          {
            
            prof_validacao=c()
            for(i in 1:nrow(x_validacao))
            {
              
              
              for(j in 1:(length(niveis_prof)-1))
              {
                if(subconjunto_folha[i]>niveis_prof[j] && subconjunto_folha[i]<=niveis_prof[j+1])
                {
                  prof_validacao[i]=j
                }
              }
            } 
            
            
          }
          #Teste
          #cbind(prof_validacao,subconjunto_folha)
          #arvore
          
          
          #Caminho exato dos índices da árvore até índice do subconjunto da árvore 
          #de uma observação
          if(tipo_poda==1)
          {
            lista_poda1=list()
            for(i in 1:nrow(x_validacao))
            {
              vetor_temporario=c()
              for(j in 0:prof_validacao[i])
              {
                vetor_temporario=append(vetor_temporario,floor(subconjunto_folha[i]/(2^j)))
              }
              lista_poda1[[i]]=vetor_temporario
            }
          }
          
          if(tipo_poda==1)
          {
            #Sequência de indices dos nós internos, do fim para o começo
            for(i in seq((2^prof_max)-1,1,-1))
            {
              if(arvore[[i]]!=0)
              {
                pos_filho_direito=c()
                pos_filho_esquerdo=c()
                for(k in 1:length(lista_poda1))
                {
                  if(is.element(2*i,lista_poda1[[k]]))
                  {
                    pos_filho_esquerdo=append(pos_filho_direito,k)
                  }else if(is.element(2*i+1,lista_poda1[[k]]))
                  {
                    pos_filho_direito=append(pos_filho_direito,k)
                  }
                }
                
                if(length(pos_filho_esquerdo)>0 && length(pos_filho_direito)>0)
                {
                  num_obs_pai=length(pos_filho_esquerdo)+length(pos_filho_direito)
                  (erro_filho_direito=(length(pos_filho_direito)/num_obs_pai)*
                      erro_medio_absoluto(y_validacao[pos_filho_direito],predict3(x_validacao[pos_filho_direito,])))
                  
                  (erro_filho_esquerdo=(length(pos_filho_esquerdo)/num_obs_pai)*
                      erro_medio_absoluto(y_validacao[pos_filho_esquerdo],predict3(x_validacao[pos_filho_esquerdo,])))
                  
                  (erro_pai=erro_medio_absoluto(y_validacao[union(pos_filho_direito,pos_filho_esquerdo)],
                                                predict3(x_validacao[union(pos_filho_direito,pos_filho_esquerdo),])))
                  
                  if(erro_pai<=(erro_filho_esquerdo+erro_filho_direito))
                  {
                    coef_modelo[[2*i+1]]=0
                    coef_modelo[[2*i]]=0
                    modelos_links[2*i+1]="removido_por_poda1"
                    modelos_links[2*i]="removido_por_poda1"
                    modelos_dist[2*i+1]="removido_por_poda1"
                    modelos_dist[2*i]="removido_por_poda1"
                    arvore[[2*i]]=0
                    arvore[[2*i+1]]=0
                    erro_menor[2*i]=-1
                    erro_menor[2*i+1]=-1
                  }
                }
              }
            }
          }
          
          
          if(tipo_poda==1)
          {
            for(i in seq(1,(2^prof_max)-1,1))
            {
              if(modelos_links[i]=="removido_por_poda1")
              {
                coef_modelo[[2*i+1]]=0
                coef_modelo[[2*i]]=0
                modelos_links[2*i+1]="removido_por_poda1"
                modelos_links[2*i]="removido_por_poda1"
                modelos_dist[2*i+1]="removido_por_poda1"
                modelos_dist[2*i]="removido_por_poda1"
                arvore[[2*i]]=0
                arvore[[2*i+1]]=0
                erro_menor[2*i]=-1
                erro_menor[2*i+1]=-1 
              }
            }
          } 
        }
      }
      
      
      #Poda com os dados de treinamento
      if(T)
      {
        if(data_frame_sep[1,1]==123456789)
        {
          print("Nao tem poda2!")
        }
        else
        {
          if(tipo_poda==2)
          {
            for(i in seq((2^prof_max)-1,1,-1))
            {
              if(erro_menor[i]!=-1 && i!= 1 && erro_menor[2*i]!=-1 && erro_menor[2*i+1]!=-1)
              {
                (erro_filho_direito=(length(arvore[[2*i+1]])/length(arvore[[i]]))*erro_menor[2*i+1]*((
                  length(arvore[[2*i+1]])+length(coef_modelo[[2*i+1]]))/(length(arvore[[2*i+1]])-
                                                                           length(coef_modelo[[2*i+1]]))))
                (erro_filho_esquerdo=(length(arvore[[2*i]])/length(arvore[[i]]))*erro_menor[2*i]*((
                  length(arvore[[2*i+1]])+length(coef_modelo[[2*i+1]]))/(length(arvore[[2*i+1]])-
                                                                           length(coef_modelo[[2*i+1]]))))
                
                if(erro_menor[i]<=(erro_filho_direito+erro_filho_esquerdo))
                {
                  coef_modelo[[2*i+1]]=0
                  coef_modelo[[2*i]]=0
                  modelos_links[2*i+1]="revomido_por_poda2"
                  modelos_links[2*i]="revomido_por_poda2"
                  modelos_dist[2*i+1]="revomido_por_poda2"
                  modelos_dist[2*i]="revomido_por_poda2"
                  arvore[[2*i]]=0
                  arvore[[2*i+1]]=0
                  erro_menor[2*i]=-1
                  erro_menor[2*i+1]=-1
                }
              }
            }
          } 
        }
        
      }
}
      
arvore
erro_medio_absoluto(y_teste,predict2(x_teste))

