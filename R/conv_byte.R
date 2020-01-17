# function to convert numeric (byte) value into pretty byte representation

conv_byte = function(size) {
  setDT(size)
  # https://stackoverflow.com/questions/29787452/how-do-i-quickly-convert-the-size-element-of-file-info-from-bytes-to-kb-mb-g
  size[ , size := utils:::format.object_size(size[ ,1], standard = 'IEC', units = 'auto')]
  out = size[ , tstrsplit(size, ' ') ]
  setnames(out, c('size', 'unit'))
  out  
}
