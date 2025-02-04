# Función---------- --------------------------------------------------------------------------------
#Recibe de entrada y salida objeto xtable evitando errores con al codificación UTF-8

message( '\tEstableciendo función tildes a latex' )

tildes_a_latex <- function( xtb_aux ) {
 #minusculas
 xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "á", "\\\'{a}", x, fixed = TRUE ) else x } ) )
 xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "é", "\\\'{e}", x, fixed = TRUE ) else x } ) )
 xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "í", "\\\'{i}", x, fixed = TRUE ) else x } ) )
 xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "ó", "\\\'{o}", x, fixed = TRUE ) else x } ) )
 xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "ú", "\\\'{u}", x, fixed = TRUE ) else x } ) )
 xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "ñ", "$\\tilde{\\text{n}}$", x, fixed = TRUE ) else x } ) )

 xtb_aux <- xtable( xtb_aux )
 
 return( xtb_aux )
}


# Función---------- --------------------------------------------------------------------------------
#Recibe de la codificación UTF-8 y devuelve codificación WIN en mayúsculas

message( '\tEstableciendo función de codificación en mayúsculas' )

cod_utf_win <- function( xtb_aux ) {
  #mayusculas
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "Á", "A", x, fixed = TRUE ) else x } ) )
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "É", "E", x, fixed = TRUE ) else x } ) )
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "Í", "I", x, fixed = TRUE ) else x } ) )
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "Ó", "O", x, fixed = TRUE ) else x } ) )
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "Ü", "U", x, fixed = TRUE ) else x } ) )
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "Ú", "U", x, fixed = TRUE ) else x } ) )
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "Ñ", "N", x, fixed = TRUE ) else x } ) )
  xtb_aux <- data.frame( lapply( xtb_aux, function( x ) { if( is.character( x ) ) gsub( "  ", " ", x, fixed = TRUE ) else x } ) )
  
  return( xtb_aux )
}

#Función para convertir la primera letra a mayúscula------------------------------------------------

firstup <- function( x ) {
  x <- tolower( x )
  substr( x, 1, 1) <- toupper(substr( x, 1, 1 ) )
  x
}