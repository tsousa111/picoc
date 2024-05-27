# PicoC
## Grupo
| Número  | Nome                  | Git             |
| :------ | :-------------------- | :-------------- |
| pg50326 | Diogo Camacho Barbosa | `DBarbosa15987` |
| pg50500 | João Tiago Sousa      | `tsousa111`     |

## Execução
No ficheiro **Runner.hs** são importadas todas as componentes do projeto.

Para testar todas as funções definidas, executar `ghci Runner.hs`.

## Propriedades
Executar `quickCheck Props.{PROP_NAME}` em que **PROP_NAME** é deve ser substituída
pela propridade desejada.

## Test Suite
As funções de teste com e sem mutações dos 3 programas pedidos estão definidas
no ficheiro **Runner.hs**.

Exemplo de execução da Suite de teste do Programa1:
`runTestSuiteProg1`

Exemplo de execução da Suite de teste com mutação do Programa1:
`runMutationSuiteProg1 SEED`
a execução desta Suite necessita do valor SEED, que deve ser substituído por um
inteiro.
