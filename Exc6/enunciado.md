Sejam X1, ..., Xn variáveis aleatórias contínuas independentes e identicamente distribuídas a X ∼ uniforme(0,1). Então Sn = ∑ni=1 Xi possui distribuição de Irwin-Hall e

P(Sn ≤ x) = 1/n!∑k=0⌊x⌋ (−1)^k.C(n,k).(x−k)^n,

para 0 ≤ x ≤ n, e onde ⌊x⌋ representa a parte inteira do real x.

    1) Obtenha o valor exato de pn = P(Sn ≤ x), para n = 12 e x = 5.75.

    2) Calcule dois valores aproximados de pn recorrendo aos métodos seguintes:

        a. Teorema do limite central (pn,TLC). Recorra ao TLC, apesar de n ser inferior a 30.

        b. Simulação (pn,sim)

            i. Fixando a semente em 5457, gere m=150 amostras de dimensão n=12 da distribuição de X.

            ii. Calcule um valor simulado de Sn para cada uma das amostras geradas.

            iii. Obtenha a proporção de valores simulados de Sn que não excedem 5.75.

    3) Determine o desvio absoluto entre o valor exato calculado em 1), pn, e o valor aproximado obtido em 2a., pn,TLC.

    4) Calcule o desvio absoluto entre pn e pn,sim.

    5) Calcule o quociente entre os desvios calculados em 3. e 4. e apresente o resultado arredondado a 4 casas decimais.