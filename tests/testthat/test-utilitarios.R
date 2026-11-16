test_that("normalizar deixa o texto em caixa alta e sem acentos", {
  expect_equal(
    normalizar(c("Olá, mundo", "  r. do açaí 15º  ")),
    c("OLA, MUNDO", "R. DO ACAI 15O")
  )
})

test_that("normalizar preserva NA e transforma vazio em NA", {
  expect_equal(normalizar(c(NA, "")), c(NA_character_, NA_character_))
})

test_that("e_dado_faltante identifica ausência de informação", {
  expect_equal(
    e_dado_faltante(c("SI", "SEM INFORMACAO", "NAO CONSTA", "RUA B", NA)),
    c(TRUE, TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("e_dado_faltante não trata S/N como dado faltante", {
  expect_false(e_dado_faltante("S/N"))
})

test_that("zerar_dado_faltante devolve NA e normaliza o resto", {
  expect_equal(
    zerar_dado_faltante(c("SI", "RUA B", "  São Gonçalo ")),
    c(NA, "RUA B", "SAO GONCALO")
  )
})

test_that("padronizar_metaphone aproxima grafias parecidas", {
  expect_equal(padronizar_metaphone(c("Silva", "Sylva")), c("SILVA", "SILVA"))
})

test_that("numero_por_extenso escreve números em português", {
  expect_equal(
    numero_por_extenso(c(1L, 21L, 405L)),
    c("UM", "VINTE E UM", "QUATROCENTOS E CINCO")
  )
})

test_that("numero_por_extenso lida com NA e vetor vazio", {
  expect_equal(numero_por_extenso(c(1L, NA)), c("UM", NA_character_))
  expect_equal(numero_por_extenso(integer()), character())
})

test_that("romano_para_inteiro converte algarismos romanos", {
  expect_equal(romano_para_inteiro(c("XIV", "MMXXVI", "IV")), c(14L, 2026L, 4L))
})

test_that("romano_para_inteiro devolve 0 para o que não é romano", {
  expect_equal(romano_para_inteiro("banana"), 0L)
})

test_that("padronizar_numeros_por_extenso escreve os dígitos por extenso", {
  expect_equal(
    padronizar_numeros_por_extenso(c("RUA 2", "Av. 5 de Julho, 22")),
    c("RUA DOIS", "Av. CINCO de Julho, VINTE E DOIS")
  )
})

test_that("padronizar_numeros_por_extenso preserva texto sem dígitos e NA", {
  expect_equal(
    padronizar_numeros_por_extenso(c("Sem números", NA, "")),
    c("Sem números", NA_character_, NA_character_)
  )
})

test_that("padronizar_numero_romano_por_extenso escreve os romanos por extenso", {
  expect_equal(
    padronizar_numero_romano_por_extenso(c("Capítulo IX", "Rua Pedro II")),
    c("Capítulo NOVE", "Rua Pedro DOIS")
  )
})

test_that("padronizar_numero_romano_por_extenso preserva palavra sem romano", {
  expect_equal(padronizar_numero_romano_por_extenso("banda"), "banda")
})

test_that("as utilidades de texto validam a entrada", {
  expect_error(normalizar(1))
  expect_error(e_dado_faltante(1))
  expect_error(zerar_dado_faltante(1))
  expect_error(padronizar_metaphone(1))
  expect_error(numero_por_extenso("um"))
  expect_error(romano_para_inteiro(1))
  expect_error(padronizar_numeros_por_extenso(1))
  expect_error(padronizar_numero_romano_por_extenso(1))
})
