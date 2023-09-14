# Credibilidade na medida de probabilidade

**Resumo:** 
Este estudo busca organizar a interpretação e a medida de credibilidade, principalmente no contexto monetário. Propomos um conceito abrangente, baseado na linguagem natural e que aninha diversas interpretações, formalizando-o matematicamente na estrutura da Teoria da Probabilidade, na qual também construiu-se a medida. Então realizamos uma aplicação empírica do conceito e da medida. Utilizando dados de um survey de expectativas de inflação (pontuais e probabilísticas) para a economia dos Estados Unidos, estimamos credibilidades para variados horizontes temporais e focos. Métodos
não-paramétricos (spline e kernel), com especificações adaptadas à particularidade dos dados, foram empregados nas estimações. Os resultados são apresentados em gráficos informativos.

**Palavras-chave:** credibilidade; probabilidade; expectativas; inflação

***

Todas os cálculos e análises foram feitos com algorítmos elaborado pelo autor, em R.

***


Este trabalho objetiva unir algumas interpretações e medidas de credibilidade na área monetária, propondo um conceito abrangente com uma medida de credibilidade facilmente tratável, bem como apresentar duas estratégias empíricas, realizando uma aplicação para a meta de inflação da economia dos Estados Unidos. Cremos que essa nova formalização do conceito, junto com o exemplo aplicado, possa contribuir para o avanço das teorias macroeconômicas relacionadas e para o estudo empírico principalmente das crenças que envolvem políticas econômicas, não apenas da área monetária.

Nossa metodologia calcula as credibilidades tanto com dados de expectativas probabilísticas quanto de expectativas pontuais. Para os primeiros, muito mais ricos informacionalmente, construímos fdps individuais e do agente médio, no formato uniforme por intervalos e no formato suave (para o agente médio), todas empregando splines. Para o formato suave, dada a peculiaridade dos dados, utilizamos um modelo diferenciado para a estimação, minimizando a soma do quadrado dos erros de probabilidade (SSPE); este, também usado em uma generalized cross validation adaptada. Os dados de expectativas pontuais proporcionaram a construção de fdps para o agente médio, na qual fizemos uso da kernel density estimation supondo uma mesma distribuição de probabilidade entre os agentes.

Figura 2: Densidades e credibilidade:

[img2.pdf](https://github.com/raguirreleal/credibilidade/files/12602629/img2.pdf)

Figura 3: Credibilidades por horizontes e tipos de dados:

[img3.pdf](https://github.com/raguirreleal/credibilidade/files/12602632/img3.pdf)

Figura 4: Credibilidades de período-alvo fixo

[img4.pdf](https://github.com/raguirreleal/credibilidade/files/12602633/img4.pdf)


