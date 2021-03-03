import numpy as np
import pandas as pd
from sklearn.model_selection import RepeatedKFold,RandomizedSearchCV
from sklearn.ensemble import RandomForestRegressor,GradientBoostingRegressor
from sklearn.linear_model import Ridge,Lasso,TweedieRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import mean_absolute_error
from scipy.stats import uniform
from sklearn.feature_selection import SelectKBest,  f_regression


#Dados Iris
x_treino=pd.read_csv("iris_treino.csv")
x_teste=pd.read_csv("iris_teste.csv")
y_treino=x_treino.values[:,0]
y_teste=x_teste.values[:,0]

x_treino=x_treino.values[:,[1,2,3]]
x_teste=x_teste.values[:,[1,2,3]]


#Dados mpg
x_treino=pd.read_csv("autompg_treino.csv")
x_teste=pd.read_csv("autompg_teste.csv")
x_treino.columns
y_treino=x_treino.values[:,5]
y_teste=x_teste.values[:,5]

x_treino=x_treino.values[:,[0,1,2,3,4,6,7]]
x_teste=x_teste.values[:,[0,1,2,3,4,6,7]]

#Dados slump test
x_treino=pd.read_csv("slump_treino.csv")
x_teste=pd.read_csv("slump_teste.csv")
x_treino.columns
y_treino=x_treino.values[:,9]
y_teste=x_teste.values[:,9]

x_treino=x_treino.values[:,np.arange(9)]
x_teste=x_teste.values[:,np.arange(9)]

#Dados parkinson
x_treino=pd.read_csv("parkinson_treino.csv")
x_teste=pd.read_csv("parkinson_teste.csv")
x_treino.columns
y_treino=x_treino.values[:,4]
y_teste=x_teste.values[:,4]

x_treino=x_treino.values[:,np.setdiff1d(np.arange(22),4)]
x_teste=x_teste.values[:,np.setdiff1d(np.arange(22),4)]

#Dados preço das casas(Real estate valuation)
x_treino=pd.read_csv("real_state_treino.csv")
x_teste=pd.read_csv("real_state_teste.csv")
x_treino.columns
y_treino=x_treino.values[:,5]
y_teste=x_teste.values[:,5]

x_treino=x_treino.values[:,np.arange(5)]
x_teste=x_teste.values[:,np.arange(5)]

#Dados de toxicidade aquática
x_treino=pd.read_csv("qsar_treino.csv")
x_teste=pd.read_csv("qsar_teste.csv")
x_treino.columns
y_treino=x_treino.values[:,2]
y_teste=x_teste.values[:,2]

x_treino=x_treino.values[:,[0,1,3,4,6]]
x_teste=x_teste.values[:,[0,1,3,4,6]]

#Dados de energia de construções simuladas
x_treino=pd.read_csv("energy_treino.csv")
x_teste=pd.read_csv("energy_teste.csv")
x_treino.columns
y_treino=x_treino.values[:,8]
y_teste=x_teste.values[:,8]

x_treino=x_treino.values[:,np.arange(8)]
x_teste=x_teste.values[:,np.arange(8)]




#Ajuste dos modelos
k=RepeatedKFold(n_splits=3, n_repeats=5, random_state=0)

#Seleção de variávies
k_vs_mae = []
indice_i=[]
#k=RepeatedKFold(n_splits=5,n_repeats=3)
#warnings.filterwarnings("ignore")
for i in range(1,x_treino.shape[1]+1,1):
    score=[]
    dp=[]
    for treino_index, teste_index in k.split(x_treino):        
        xtreino=x_treino[treino_index,:]
        xteste=x_treino[teste_index,:]
        ytreino= y_treino[treino_index]
        yteste=y_treino[teste_index]
        selector = SelectKBest(score_func=f_regression, k=i)
        xtreino2=selector.fit_transform(xtreino,ytreino)
        xteste2=selector.transform(xteste)
        
        mdl = GradientBoostingRegressor()
        mdl.fit(xtreino2, ytreino)
    
        p = mdl.predict(xteste2)
    
        score.append(mean_absolute_error(yteste, p))
    print("k = {} - EMA = {} - DP = {}".format(i, np.mean(score), np.std(score)))

    k_vs_mae.append(np.mean(score))
    indice_i.append(i)

np.min(k_vs_mae)
indice_i[np.argmin(k_vs_mae)]
pd.Series(k_vs_mae, index=range(1,x_treino.shape[1]+1,1)).plot(figsize=(10,7))

selector = SelectKBest(score_func=f_regression, k=3)
selector.fit(x_treino, y_treino)

mask = selector.get_support()

x_2=x_treino[:,mask]
x_teste=x_teste[:,mask]
#x_2=x_treino
#Modelos
#MLG Normal, MLG Gama, MLG Normal Inversa
modelo1=TweedieRegressor(power=0)
modelo2=TweedieRegressor(power=2)
modelo3=TweedieRegressor(power=3)
erro1,erro2,erro3=([],[],[])

for treino_index, teste_index in k.split(x_treino):
    xtreino=x_2[treino_index,:] 
    xteste=x_2[teste_index,:]
    ytreino= y_treino[treino_index] 
    yteste=y_treino[teste_index]
    modelo1.fit(xtreino,ytreino)
    modelo2.fit(xtreino,ytreino)
    modelo3.fit(xtreino,ytreino)
    
    erro_medio_absoluto1=mean_absolute_error(yteste,modelo1.predict(xteste))
    erro_medio_absoluto2=mean_absolute_error(yteste,modelo2.predict(xteste))
    erro_medio_absoluto3=mean_absolute_error(yteste,modelo3.predict(xteste))
    
    erro1.append(erro_medio_absoluto1)
    erro2.append(erro_medio_absoluto2)
    erro3.append(erro_medio_absoluto3)

print(np.mean(erro1))
print(np.mean(erro2))
print(np.mean(erro3))

tabela_media=[np.mean(erro1),np.mean(erro2),np.mean(erro3)]
desvio=[np.std(erro1),np.std(erro2),np.std(erro3)]
hiper_algoritmos=["Nenhum"]*3

#Regressão Ridge
modelo=Ridge()
#uniform(loc=0, scale=4).rvs(size=10)
hiperp = {"alpha":uniform(loc=0,scale=7)}
Otimizacao = RandomizedSearchCV(modelo, hiperp,cv=k, n_iter = 30
                                ,scoring="neg_mean_absolute_error", random_state=0)
Otimizacao.fit(x_2, y_treino)
Otimizacao.best_params_
modelo=Ridge(**Otimizacao.best_params_)
modelo.fit(x_2, y_treino)

erro=mean_absolute_error(y_teste, modelo.predict(x_teste))
tabela_media.append(erro)
hiper_algoritmos.append(Otimizacao.best_params_)

#Regressão Lasso
modelo=Lasso()
hiperp = {"alpha":uniform(loc=0,scale=7)}
Otimizacao = RandomizedSearchCV(modelo, hiperp,cv=k, n_iter = 30
                                ,scoring="neg_mean_absolute_error", random_state=0)
Otimizacao.fit(x_2, y_treino)
Otimizacao.best_params_
modelo=Lasso(**Otimizacao.best_params_)
modelo.fit(x_2, y_treino)

erro=mean_absolute_error(y_teste, modelo.predict(x_teste))
tabela_media.append(erro)
hiper_algoritmos.append(Otimizacao.best_params_)

#Floresta Aleatória
modelo=RandomForestRegressor()
hiperp = {"n_estimators":[100,200,250],"max_depth":[2,3,4],
                 "min_samples_split":[10,15,20,30]
                 ,"min_samples_leaf":[5,10,15,20,25,30,50]}
Otimizacao = RandomizedSearchCV(modelo, hiperp,cv=k, n_iter = 30
                                ,scoring="neg_mean_absolute_error", random_state=0)
Otimizacao.fit(x_2, y_treino)
Otimizacao.best_params_
modelo=RandomForestRegressor(**Otimizacao.best_params_)
modelo.fit(x_2, y_treino)

erro=mean_absolute_error(y_teste, modelo.predict(x_teste))
tabela_media.append(erro)
hiper_algoritmos.append(Otimizacao.best_params_)

#Árvore de Regressão
modelo=DecisionTreeRegressor()
hiperp = {"max_depth":[2,3,4],
                 "min_samples_split":[10,15,20,30]
                 ,"min_samples_leaf":[5,10,15,20,25,30,50]}
Otimizacao = RandomizedSearchCV(modelo, hiperp,cv=k, n_iter = 30
                                ,scoring="neg_mean_absolute_error", random_state=0)
Otimizacao.fit(x_2, y_treino)
Otimizacao.best_params_
modelo=DecisionTreeRegressor(**Otimizacao.best_params_)
modelo.fit(x_2, y_treino)

erro=mean_absolute_error(y_teste, modelo.predict(x_teste))
tabela_media.append(erro)
hiper_algoritmos.append(Otimizacao.best_params_)

#Gradient tree
modelo=GradientBoostingRegressor()
hiperp = {"n_estimators":[100,200,250],"max_depth":[2,3,4],
                 "min_samples_split":[10,15,20,30]
                 ,"min_samples_leaf":[5,10,15,20,25,30,50]}
Otimizacao = RandomizedSearchCV(modelo, hiperp,cv=k, n_iter = 30
                                ,scoring="neg_mean_absolute_error", random_state=0)
Otimizacao.fit(x_2, y_treino)
Otimizacao.best_params_
modelo=GradientBoostingRegressor(**Otimizacao.best_params_)
modelo.fit(x_2, y_treino)

erro=mean_absolute_error(y_teste, modelo.predict(x_teste))
tabela_media.append(erro)
hiper_algoritmos.append(Otimizacao.best_params_)


tabela_media
hiper_algoritmos
#Intervalo de confiança
#LI=np.array(tabela_media)-1.959964*np.array(desvio)/20
#LS=np.array(tabela_media)+1.959964*np.array(desvio)/20
tabela_algoritmos_python=pd.DataFrame(np.column_stack((tabela_media,
hiper_algoritmos)),index=[
       "MLG Normal","MLG Gama","MLG Normal Inversa",
        "Regressao Ridge","Regressao Lasso","Floresta Aleatoria",
        "Arvore de Regressao","Gradient Tree Regressor"]
 ,columns=["Erro","Hip"])
tabela_algoritmos_python

tabela_algoritmos_python.to_csv(path_or_buf="\\Users\\Alberto\\Desktop\\Trabalhos\\Monografia\\artigos_amg\\resultados_python_parkinson.csv")