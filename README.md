# Rotinas AutoLISP — Civil 3D / Topografia

Documentação das rotinas LISP deste repositório, organizadas por pasta.

> **Nota:** a pasta `Lisps/` é uma cópia da estrutura raiz (`Area/`, `Pontos/`, `Postes/`, `Secoes/`). A documentação abaixo vale para as duas.

---

## 📁 Area — Anotação de áreas de hatch (seções transversais)

Todas as rotinas desta pasta medem a área de hatches (aterro, corte, reaterro, rocha) em seções transversais, inserem textos de anotação no desenho e, em algumas versões, gravam os valores em arquivo TXT (formato `estaca;área;área;...` com separador `;` e decimal com vírgula). Os textos usam estilo **ARIAL** (ou Standard, se não existir) com altura 0.4.

### HatchAnnotate_Base.lsp — comando `HT`
Versão mais simples. Seleciona **duas entidades** livremente (sem validar layer), pega a área de cada uma e insere dois textos abaixo do ponto clicado, no formato `<layer> = <área> m²`. Se a entidade não for selecionada, insere `ÁREA CORTE = 0,00 m²` / `ÁREA ATERRO = 0,00 m²`. Não grava arquivo.

### HatchAnnotate_txtBase.lsp — comando `HTxt`
Versão com registro em TXT. Fluxo:
1. Seleciona o **texto da estaca** (TEXT ou MTEXT);
2. Clica o ponto de inserção das anotações;
3. Seleciona os hatches — identifica automaticamente pelo layer: `Aterro` ou `Corte` (ENTER finaliza);
4. Insere os textos `ÁREA ATERRO = ...` (cor 40) e `ÁREA CORTE = ...` (cor 20);
5. Grava a linha `estaca;aterro;corte;` no arquivo **`areas_estacas.txt`** (na pasta do DWG, modo append).

### HatchAnnotate_txtAterroCorte2.0.lsp — comando `HTxt` (versão 2.0)
Evolução da `txtBase`, a versão mais robusta da família:
- Valida que a estaca selecionada é TEXT/MTEXT e que os objetos clicados são HATCH;
- Trata clique no vazio (errno 7) sem sair do loop; ENTER/ESC finaliza;
- Sai sozinho quando os dois hatches (Aterro e Corte) já foram identificados;
- **Cancela a operação** se aterro e corte forem ambos 0,00 (não insere texto nem grava);
- Grava em **`areas_estacas.txt`**.

### HatchAnnotate_txtAterroCorteRocha_Base.lsp — comando `HT`
Anota **três hatches** com validação estrita de layer, um por vez (ENTER = 0,00):
1. `ÁREA ATERRO` (cor 40);
2. `ÁREA CORTE 1ª e 2ª Cat.` (cor 20);
3. `ÁREA CORTE 3ª Cat.` — rocha (cor 22).

Só insere os textos no desenho; **não grava arquivo**.

### HatchAnnotate_txtAterroCorteRocha1.5.lsp — comando `HTxt` (versão 1.5)
Mesma seleção tripla (aterro / corte 1ª-2ª / corte 3ª) da versão acima, mas com estaca e gravação em TXT:
- Insere os três textos (o de CORTE 3ª fica deslocado 10,5 unidades à direita, na mesma linha do corte 1ª-2ª);
- Grava `estaca;aterro;corte12;corte3` em **`areas_estacas.txt`**.

### HatchAnnotate_txtCorte.lsp — comando `3CAT`
Anota apenas o hatch de **CORTE 3ª categoria** (rocha). Exige layer `ÁREA CORTE 3ª Cat.` (ENTER = 0,00). Insere o texto (cor 22) e grava `estaca;área` em **`areas_hatch2.txt`**.

### HatchAnnotate_txtRocha.lsp — comando `3CAT`
Praticamente idêntica à `txtCorte` (mesmo comando, mesmo layer `ÁREA CORTE 3ª Cat.`, mesmo arquivo `areas_hatch2.txt`). Diferença: também formata `area1`/`area2` de variáveis não usadas — é uma variante intermediária da mesma rotina.

### HatchAnnotate_txtReaterro.lsp — comando `3CAT`
Variante para **REATERRO**: exige layer `ÁREA REATERRO`, insere o texto `ÁREA REATERRO = ... m²` (cor 52) e grava `estaca;área` em **`areas_hatch2.txt`**.

> ⚠️ **Atenção:** `txtCorte`, `txtRocha` e `txtReaterro` definem o **mesmo comando `3CAT`** — o último arquivo carregado sobrescreve os anteriores. Carregue apenas o que for usar.

---

## 📁 Pontos — COGO Points e verificação de duplicados

### PontosAcesso.lsp — comandos `PONTOS_ACESSO_EIXO`, `PONTOS_ACESSO_ESQ`, `PONTOS_ACESSO_DIR`
Gera as linhas de um acesso a partir de COGO Points levantados em **seções de 3 pontos** (esquerda, eixo, direita). Ordena os pontos por Northing (avanço), agrupa de 3 em 3 e ordena cada grupo por Easting:
- `PONTOS_ACESSO_EIXO` — cria uma 3DPOLY ligando o **ponto central** de cada seção;
- `PONTOS_ACESSO_ESQ` — cria a 3DPOLY do **bordo esquerdo** (menor X de cada seção);
- `PONTOS_ACESSO_DIR` — cria a 3DPOLY do **bordo direito** (maior X de cada seção).

### PontosAcessoLV.lsp — comandos `PONTOS_ACESSO_LV_EIXO`, `PONTOS_ACESSO_LV_ESQ`, `PONTOS_ACESSO_LV_DIR`
Versão para levantamento com seções de **2 pontos** (bordo esquerdo e direito, sem ponto de eixo):
- `PONTOS_ACESSO_LV_EIXO` — filtra somente os COGO Points com **Raw Description = "EIX"**, ordena por avanço e liga em 3DPOLY; interrompe a linha quando a distância entre pontos consecutivos passa de **25 m** (evita ligar trechos separados);
- `PONTOS_ACESSO_LV_ESQ` / `PONTOS_ACESSO_LV_DIR` — agrupam de 2 em 2 e ligam o ponto de menor/maior X de cada dupla.

### PontosPeCrista.lsp — comandos `PontosA2`, `PONTOS_PE_CRISTA`
Liga COGO Points de linhas de **pé e crista de talude** em 3DPOLYs, "seguindo" a direção natural da linha:
- `PontosA2` — parte do ponto mais ao sul e vai encadeando o vizinho mais próximo que atenda aos limites: distância ≤ **22 m**, desvio angular ≤ **70°** e afastamento lateral ≤ **4,5 m**. Cria quantas 3DPOLYs forem necessárias até usar todos os pontos;
- `PONTOS_PE_CRISTA` — mesmo princípio (distância ≤ 22 m, ângulo ≤ 30°), mas pede também uma seleção de **linhas-barreira** (LINE/POLYLINE) que os segmentos não podem cruzar (ex.: o eixo) — usa teste de interseção de segmentos para impedir que a linha de pé cruze para o lado da crista.

### PontosRepetidos.lsp — comando `CheckCogoDup`
Varre **todos** os COGO Points do desenho e detecta pontos na mesma posição (tolerância de 0,001). Os duplicados são adicionados à seleção corrente (ficam com grips ativos) e o total é informado no prompt.

### PlRepetida.lsp — comando `CheckDup3DPoly`
Nas polylines selecionadas, detecta **3DPOLYs duplicadas** — mesma sequência de vértices (tolerância 0,001), inclusive em **ordem invertida**. As duplicadas são pintadas de **verde** (cor 3) e o total é informado.

### VerticeDup.lsp — comando `CheckVertDup3D`
Nas polylines selecionadas, detecta 3DPOLYs que possuem **vértices repetidos dentro da própria linha** (tolerância 0,001). As polylines com problema são pintadas de **verde** e o total é informado.

---

## 📁 Postes — Amarração de postes ao eixo (alignment)

### Postes.lsp — comandos `POSTES_TXT`, `POSTES_CSV`, `LAYERTOSHEET`
Calcula estaca + offset de postes em relação a um **Alignment** do Civil 3D (estaqueamento de 20 m):
- `POSTES_TXT` — seleciona o eixo e, a cada clique num poste, imprime no prompt: estaca (`N+f.fff`), offset, lado (E/D), Norte e Este. Só exibe, não grava;
- `POSTES_CSV` — mesma lógica, mas acumula as linhas e gera o arquivo **`postes_eixo_<estacaInicial>_<estacaFinal>.csv`** na pasta do DWG, com colunas `INICIAL;+;FRACAO;NORTE;ESTE;LADO;DISTANCIA` (decimais com vírgula). Tem tratamento de erro: se o comando for interrompido (ESC), salva o CSV com o que já foi clicado;
- `LAYERTOSHEET` — utilitário: clica em um objeto e grava o **nome do layer** dele no arquivo `layers_export.csv` (append).

O arquivo também define as funções auxiliares `lado-offset` (esquerda/direita via produto vetorial com a tangente do eixo — redefinida várias vezes ao longo do arquivo; vale a última: retorna `LE`/`LD`), `ponto->virgula` e `salvar-csv`.

### InicioFinal.lsp — comando `InicioFim`
Registra **trechos** (início/fim) amarrados ao eixo. Para cada trecho:
1. Seleciona um COGO Point cuja Raw Description contenha **"INICIAL"** e outro com **"FINAL"** (valida e insiste até acertar);
2. Projeta os dois no alignment e calcula estaca + fração (20 m) de cada um;
3. Determina o lado (LE/LD) pelo ponto inicial.

O loop roda até o usuário apertar **ESC** — aí gera o CSV `postes_eixo_<ini>_<fim>.csv` com colunas `INICIAL;+;FRACAO;FINAL;+;FRACAO;LADO`.

> ⚠️ **Dependência:** `InicioFim` usa `ponto->virgula`, `lado-offset` e `salvar-csv`, que estão definidas em `Postes.lsp` — carregue `Postes.lsp` antes.

---

## 📁 Secoes — Geração de linhas em seções transversais

### Secoes.lsp — comando `SECOES`
Gera uma polyline de "terreno primitivo" entre o terreno natural e a pista em uma seção transversal. Em loop contínuo (ESC encerra):
1. Seleciona a PL do **terreno natural**, a PL da **pista** e a PL do **eixo** (vertical);
2. Pede o **percentual de aumento da área** (padrão 75%);
3. Gera abscissas com espaçamento aleatório de 4 a 6 m (mínimo 3 m entre pontos) mais o X do eixo;
4. Em cada X, calcula a nova cota afastando-a da pista: `Ynovo = Ypista + (Ytn − Ypista) × (1 + perc/100)` — ou seja, amplia a diferença TN−pista pelo fator informado;
5. O primeiro e o último ponto coincidem com o terreno natural. Desenha a PLINE resultante.

### Secoes2.lsp — comando `SDCOES`
Evolução da `SECOES`. Mesma lógica, com dois pontos extras "de amarração": logo após o primeiro ponto (e antes do último), insere um ponto **sobre a extremidade da pista** com offset vertical de ±0,01 — para baixo se a pista está abaixo do TN, para cima se está acima. Garante que a nova linha "cole" nos offs da pista.

### SecoesLinhaProjeto3.lsp — comando `SDCOES` (versão 3)
Versão com **validação de layer** nas seleções:
- Terreno natural: PL no layer `F-SC-VIEW`;
- Pista: PL no layer `F-SC-PROJETO`;
- Eixo: **LINE** no layer `F-SC-MALHA-TXT`.

O offset das pontas (±0,003) é decidido pela **inclinação do bordo da pista** (compara o vértice da ponta com o vizinho): se o bordo desce, o ponto sobe, e vice-versa. Restante igual à `Secoes2`.

> ⚠️ `Secoes2.lsp` e `SecoesLinhaProjeto3.lsp` definem o **mesmo comando `SDCOES`** — carregue apenas um.

### SecoesFundoRocha(Offset).lsp — comando `ZLINHA_RANDOMICA`
Gera a linha de **fundo de rocha** deslocada de uma linha base. Em loop contínuo (ESC sai):
1. Seleciona a **linha base** (LINE ou PL) e a **LINE do eixo**;
2. Divide a base em 5 a 8 segmentos e desloca cada ponto perpendicularmente para baixo com offset aleatório (~0,2 a 0,5), dando aspecto natural;
3. O ponto central é forçado no X do eixo, com Y suavizado (média dos vizinhos);
4. Ajusta as pontas para ficarem pelo menos 0,2 acima do ponto vizinho (evita ponta "caída");
5. Desenha a PLINE.

### SecoesFundoRochaAjuste.lsp — comando `QSubir`
Ajusta uma linha (ex.: fundo de rocha) **aproximando-a da linha de projeto**. Em loop contínuo:
1. Pede o **fator de aproximação** (0.0 a 1.0, padrão 0.5);
2. Seleciona a linha **NOVA** e a linha do **PROJETO**;
3. Para cada vértice da nova: se já está a menos de 0,5 do projeto, mantém; senão aproxima o Y pelo fator, respeitando distância mínima de **0,37** do projeto (nunca cruza nem encosta);
4. Desenha a PLINE ajustada (a original é mantida).

### SecoesNovaRocha.lsp — comando `NROCHA`
Gera automaticamente uma **nova linha de rocha** (linha de topo) em cada seção: uma **cúpula lisa** que sobe da rocha existente em direção ao TN. A nova linha começa e termina nos **mesmos extremos da rocha existente** (fechamento natural por baixo) e, no meio, sobe até uma **fração do vão rocha→TN** (`*ALTURA-FRAC*`, padrão 0.60 = 60%), formando um arco liso (corda entre as pontas + curva senoidal) — sem seguir as ondulações da rocha, sem passar do TN (`*TN-GAP*` de folga) e sem subir pelas laterais/taludes. Em loop contínuo (ESC/clique no vazio encerra):
1. Seleção **por clique**, validando só o tipo (imprime a layer de cada objeto para conferência), na ordem: **TN → Projeto → Rocha**;
2. Só cria rocha em **corte** (onde o TN está acima do projeto);
3. Desenha a cúpula no layer `*LAYER-NOVA*` (cor 22) e **imprime** a área de rocha original, a nova e o % de aumento (referência).

> ℹ️ A **altura da cúpula é controlada diretamente** por `*ALTURA-FRAC*` — aumente para uma cúpula mais alta (mais perto do TN), diminua para mais baixa. Se a área original sair ~0, você clicou na linha errada (ex.: o TN em vez da rocha).

### SCTOPO.lsp — comando `SCTOPO`
Exporta seções transversais desenhadas em CAD para arquivo **.TGS** (formato de seções do Topograph). Pergunta se o arquivo é **Novo** ou **Continuação** (append) e grava o cabeçalho `*1`. Em loop (ENTER sai), para cada seção:
1. Seleciona o **texto da estaca** e o **texto da cota** de referência;
2. Clica o **ponto de referência do eixo**;
3. Seleciona a polyline do **terreno natural**;
4. Para cada vértice (ordenado por X), calcula o offset horizontal em relação ao eixo e a cota real (cota do texto + diferença de Y) e grava no formato TGS: `>estaca  nºpontos` seguido de `índice  offset  cota`.

---

## 📄 teste.lsp — comando `LH`
Utilitário de conferência: seleciona hatches e, em cada hatch com **área > 100**, desenha uma **linha verde vertical** de 100 unidades a partir do centro do bounding box — serve para marcar visualmente hatches grandes. O comprimento é ajustável na variável `comprimento` no início do arquivo.
