(ns batalha-naval2.0.teste2)

(defrecord Jogo [tabuleiro recorde])

(def recorde-atom (atom nil))

(defn posicao-valida? [linha coluna]
  (and (>= linha 0) (< linha 4)
       (>= coluna 0) (< coluna 4)))

(defn inicializar-tabuleiro []
  (vec (repeat 4 (vec (repeat 4 \~)))))

(defn imprimir-tabuleiro [tabuleiro]
  (doseq [linha tabuleiro]
    (println (apply str (map #(cond (= % :navio) \~ (= % :encontrado) "X" :else %) linha)))))

(defn posicionar-navios-aleatorios [tabuleiro num-navios]
  (loop [tabuleiro-atual tabuleiro
         navios-posicionados 0]
    (if (= navios-posicionados num-navios)
      tabuleiro-atual
      (let [linha (rand-int (count tabuleiro))
            coluna (rand-int (count (first tabuleiro)))
            novo-tabuleiro (assoc-in tabuleiro-atual [linha coluna] :navio)]
        (recur novo-tabuleiro (inc navios-posicionados))))))

(defn jogada-valida? [tabuleiro linha coluna]
  (and (posicao-valida? linha coluna)
       (let [posicao (get-in tabuleiro [linha coluna])]
         (not (or (= posicao :encontrado) (= posicao nil) (= posicao \*))))))

(defn algum-navio-encontrado? [tabuleiro num-navios]
  (>= (count (filter #(= :encontrado %) (apply concat tabuleiro))) num-navios))

(defn formatted-time [time]
  (quot time 1000))

(defn- atualizar-recorde [recorde jogadas tempo]
  (let [jogadas-recorde (or (:jogadas recorde) Integer/MAX_VALUE)
        tempo-recorde (or (:tempo recorde) Long/MAX_VALUE)
        novo-recorde {:jogadas jogadas :tempo tempo}
        quebrou-recorde? (or (< jogadas jogadas-recorde)
                             (and (= jogadas jogadas-recorde)
                                  (< tempo tempo-recorde)))]
    (println "Recorde Anterior: Jogadas: " jogadas-recorde ", Tempo: " (formatted-time tempo-recorde))
    (when quebrou-recorde?
      (reset! recorde-atom novo-recorde))
    (if quebrou-recorde? novo-recorde recorde)))

(defn jogo-concluido [tabuleiro jogadas]
  (println "Parabéns! Você encontrou os navios!")
  (let [tempo-jogo (System/currentTimeMillis)
        recorde (deref recorde-atom)
        novo-recorde (atualizar-recorde recorde jogadas tempo-jogo)
        mensagem-recorde (if (= recorde novo-recorde)
                           "Recorde não alcançado."
                           "Você atingiu um novo recorde!")]
    (println mensagem-recorde)
    (imprimir-tabuleiro tabuleiro)
    (println "Jogo Concluído!")
    (println "Recorde Atual : Jogadas: " (:jogadas novo-recorde) ", Tempo: " (formatted-time (:tempo novo-recorde)))))

(defn jogo-loop [jogadas tabuleiro num-navios-vitoria]
  (loop [jogadas jogadas
         tabuleiro tabuleiro]
    (let [tabuleiro-visual (vec (for [linha tabuleiro]
                                  (mapv #(if (= % :navio) \~ %) linha)))]
      (println "\nJogadas: " jogadas)
      (imprimir-tabuleiro tabuleiro-visual)

      (if (algum-navio-encontrado? tabuleiro num-navios-vitoria)
        (do
          (jogo-concluido tabuleiro jogadas)
          {:tabuleiro tabuleiro :encerrado true})
        (let [entrada (read-line)]
          (if (= "desistir" entrada)
            (do
              (println "Você desistiu do jogo. Até a próxima!")
              {:tabuleiro tabuleiro :encerrado true})
            (if-let [[linha coluna] (and (not (empty? entrada))
                                         (map read-string (clojure.string/split entrada #" ")))]
              (if (jogada-valida? tabuleiro linha coluna)
                (if (= :navio (get-in tabuleiro [linha coluna]))
                  (do
                    (println "Você encontrou um navio!")
                    (recur (inc jogadas) (assoc-in tabuleiro [linha coluna] :encontrado)))
                  (do
                    (println "Água! Tente novamente.")
                    (let [novo-tabuleiro (assoc-in tabuleiro [linha coluna] \*)]
                      (recur (inc jogadas) novo-tabuleiro))))
                (do
                  (println "Posição inválida ou já tentada. Tente novamente.")
                  (recur jogadas tabuleiro))))))))))

(defn jogar-batalha-naval []
  (println "Bem-vindo ao Jogo de Batalha Naval!")
  (println "Objetivo: Encontrar os navios em menos jogadas e menos tempo.")
  (println "Para desistir, digite 'desistir'.")
  (println "Para fazer uma jogada, digite as coordenadas no formato 'linha coluna'. Exemplo: '1 2'.\n")
  (println "OBS: O índice das linhas e das colunas inicia em 0.")

  (let [tabuleiro-inicial (inicializar-tabuleiro)
        num-navios 2
        num-navios-vitoria 2
        tabuleiro-com-navios (posicionar-navios-aleatorios tabuleiro-inicial num-navios)
        estado-inicial {:tabuleiro tabuleiro-com-navios :encerrado false :jogadas 0 :num-navios-vitoria num-navios-vitoria}]
    (loop [estado estado-inicial]
      (if (:encerrado estado)
        (do
          (println "Jogo encerrado. Até a próxima!")
          (recur estado-inicial))
        (let [novo-estado (jogo-loop (:jogadas estado) (:tabuleiro estado) (:num-navios-vitoria estado))]
          (recur (assoc novo-estado :jogadas (inc (:jogadas novo-estado)))))))))
