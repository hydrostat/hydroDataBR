# Autenticar no HidroWebService da ANA

Autentica o usuário no serviço autenticado da ANA e retorna um objeto de
token para ser usado nas consultas que exigem credenciais. O token é
mantido apenas em memória e o método de impressão evita mostrar seu
valor sensível.

## Usage

``` r
ana_authenticate(
  identifier = NULL,
  password = NULL,
  identificador = NULL,
  senha = NULL,
  cpf_cnpj = NULL,
  base_url = ana_auth_base_url(),
  timeout = 60,
  max_attempts = 3,
  retry_sleep_seconds = 1
)
```

## Arguments

- identifier:

  Identificador de acesso ao HidroWebService da ANA. Também pode ser
  fornecido pelas variáveis de ambiente `ANA_HIDROWEBSERVICE_IDENTIFIER`
  ou `ANA_HIDRO_IDENTIFICADOR`.

- password:

  Senha de acesso ao HidroWebService da ANA. Também pode ser fornecida
  pelas variáveis de ambiente `ANA_HIDROWEBSERVICE_PASSWORD` ou
  `ANA_HIDRO_SENHA`.

- identificador:

  Alias de compatibilidade para `identifier`.

- senha:

  Alias de compatibilidade para `password`.

- cpf_cnpj:

  Alias de compatibilidade para `identifier`.

- base_url:

  URL base do serviço autenticado. Em geral, mantenha o padrão.

- timeout:

  Tempo máximo da requisição, em segundos.

- max_attempts:

  Número máximo de tentativas de autenticação.

- retry_sleep_seconds:

  Espera entre tentativas, em segundos.

## Value

Objeto de classe `ana_token`, adequado para uso nas funções de aquisição
autenticada do pacote.

## Details

Use esta função quando for consultar rotas da API autenticada, como
inventário de estações, séries diárias pela API, medições de descarga,
curvas-chave ou seções transversais. As credenciais podem ser informadas
diretamente nos argumentos ou por variáveis de ambiente, o que é mais
seguro para scripts de trabalho.

## Examples

``` r
# Exemplo operacional. Requer credenciais válidas da ANA.
if (FALSE) {
  token <- ana_authenticate(
    identifier = Sys.getenv("ANA_HIDROWEBSERVICE_IDENTIFIER"),
    password = Sys.getenv("ANA_HIDROWEBSERVICE_PASSWORD")
  )
}
```
