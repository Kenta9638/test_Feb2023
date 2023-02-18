MODULE kk_netcdf_common

IMPLICIT NONE

      integer(kind=4) nf_byte
      integer(kind=4) nf_int1
      integer(kind=4) nf_char
      integer(kind=4) nf_short
      integer(kind=4) nf_int2
      integer(kind=4) nf_int
      integer(kind=4) nf_float
      integer(kind=4) nf_real
      integer(kind=4) nf_double
      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
      integer(kind=4)           nf_fill_byte
      integer(kind=4)           nf_fill_int1
      integer(kind=4)           nf_fill_char
      integer(kind=4)           nf_fill_short
      integer(kind=4)           nf_fill_int2
      integer(kind=4)           nf_fill_int
      real(kind=4)              nf_fill_float
      real(kind=4)              nf_fill_real
      doubleprecision   nf_fill_double
      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
      integer(kind=4) nf_nowrite
      integer(kind=4) nf_write
      integer(kind=4) nf_clobber
      integer(kind=4) nf_noclobber
      integer(kind=4) nf_fill
      integer(kind=4) nf_nofill
      integer(kind=4) nf_lock
      integer(kind=4) nf_share
      integer(kind=4) nf_64bit_offset
      integer(kind=4) nf_sizehint_default
      integer(kind=4) nf_align_chunk
      integer(kind=4) nf_format_classic
      integer(kind=4) nf_format_64bit
      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)
      integer(kind=4) nf_unlimited
      parameter (nf_unlimited = 0)
      integer(kind=4) nf_global
      parameter (nf_global = 0)
      integer(kind=4) nf_max_dims
      integer(kind=4) nf_max_attrs
      integer(kind=4) nf_max_vars
      integer(kind=4) nf_max_name
      integer(kind=4) nf_max_var_dims
      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)
      integer(kind=4) nf_noerr
      integer(kind=4) nf_ebadid
      integer(kind=4) nf_eexist
      integer(kind=4) nf_einval
      integer(kind=4) nf_eperm
      integer(kind=4) nf_enotindefine
      integer(kind=4) nf_eindefine
      integer(kind=4) nf_einvalcoords
      integer(kind=4) nf_emaxdims
      integer(kind=4) nf_enameinuse
      integer(kind=4) nf_enotatt
      integer(kind=4) nf_emaxatts
      integer(kind=4) nf_ebadtype
      integer(kind=4) nf_ebaddim
      integer(kind=4) nf_eunlimpos
      integer(kind=4) nf_emaxvars
      integer(kind=4) nf_enotvar
      integer(kind=4) nf_eglobal
      integer(kind=4) nf_enotnc
      integer(kind=4) nf_ests
      integer(kind=4) nf_emaxname
      integer(kind=4) nf_eunlimit
      integer(kind=4) nf_enorecvars
      integer(kind=4) nf_echar
      integer(kind=4) nf_eedge
      integer(kind=4) nf_estride
      integer(kind=4) nf_ebadname
      integer(kind=4) nf_erange
      integer(kind=4) nf_enomem
      integer(kind=4) nf_evarsize
      integer(kind=4) nf_edimsize
      integer(kind=4) nf_etrunc
      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
      integer(kind=4)  nf_fatal
      integer(kind=4) nf_verbose
      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)
      character(len=80)   nf_inq_libvers
      external       nf_inq_libvers
      character(len=80)   nf_strerror
      external       nf_strerror
      logical        nf_issyserr
      external       nf_issyserr
      integer(kind=4)         nf_inq_base_pe
      external        nf_inq_base_pe
      integer(kind=4)         nf_set_base_pe
      external        nf_set_base_pe
      integer(kind=4)         nf_create
      external        nf_create
      integer(kind=4)         nf__create
      external        nf__create
      integer(kind=4)         nf__create_mp
      external        nf__create_mp
      integer(kind=4)         nf_open
      external        nf_open
      integer(kind=4)         nf__open
      external        nf__open
      integer(kind=4)         nf__open_mp
      external        nf__open_mp
      integer(kind=4)         nf_set_fill
      external        nf_set_fill
      integer(kind=4)         nf_set_default_format
      external        nf_set_default_format
      integer(kind=4)         nf_redef
      external        nf_redef
      integer(kind=4)         nf_enddef
      external        nf_enddef
      integer(kind=4)         nf__enddef
      external        nf__enddef
      integer(kind=4)         nf_sync
      external        nf_sync
      integer(kind=4)         nf_abort
      external        nf_abort
      integer(kind=4)         nf_close
      external        nf_close
      integer(kind=4)         nf_delete
      external        nf_delete
      integer(kind=4)         nf_inq
      external        nf_inq
      integer(kind=4)         nf_inq_ndims
      external        nf_inq_ndims
      integer(kind=4)         nf_inq_nvars
      external        nf_inq_nvars
      integer(kind=4)         nf_inq_natts
      external        nf_inq_natts
      integer(kind=4)         nf_inq_unlimdim
      external        nf_inq_unlimdim
      integer(kind=4)         nf_inq_format
      external        nf_inq_format
      integer(kind=4)         nf_def_dim
      external        nf_def_dim
      integer(kind=4)         nf_inq_dimid
      external        nf_inq_dimid
      integer(kind=4)         nf_inq_dim
      external        nf_inq_dim
      integer(kind=4)         nf_inq_dimname
      external        nf_inq_dimname
      integer(kind=4)         nf_inq_dimlen
      external        nf_inq_dimlen
      integer(kind=4)         nf_rename_dim
      external        nf_rename_dim
      integer(kind=4)         nf_inq_att
      external        nf_inq_att
      integer(kind=4)         nf_inq_attid
      external        nf_inq_attid
      integer(kind=4)         nf_inq_atttype
      external        nf_inq_atttype
      integer(kind=4)         nf_inq_attlen
      external        nf_inq_attlen
      integer(kind=4)         nf_inq_attname
      external        nf_inq_attname
      integer(kind=4)         nf_copy_att
      external        nf_copy_att
      integer(kind=4)         nf_rename_att
      external        nf_rename_att
      integer(kind=4)         nf_del_att
      external        nf_del_att
      integer(kind=4)         nf_put_att_text
      external        nf_put_att_text
      integer(kind=4)         nf_get_att_text
      external        nf_get_att_text
      integer(kind=4)         nf_put_att_int1
      external        nf_put_att_int1
      integer(kind=4)         nf_get_att_int1
      external        nf_get_att_int1
      integer(kind=4)         nf_put_att_int2
      external        nf_put_att_int2
      integer(kind=4)         nf_get_att_int2
      external        nf_get_att_int2
      integer(kind=4)         nf_put_att_int
      external        nf_put_att_int
      integer(kind=4)         nf_get_att_int
      external        nf_get_att_int
      integer(kind=4)         nf_put_att_real
      external        nf_put_att_real
      integer(kind=4)         nf_get_att_real
      external        nf_get_att_real
      integer(kind=4)         nf_put_att_double
      external        nf_put_att_double
      integer(kind=4)         nf_get_att_double
      external        nf_get_att_double
      integer(kind=4)         nf_def_var
      external        nf_def_var
      integer(kind=4)         nf_inq_var
      external        nf_inq_var
      integer(kind=4)         nf_inq_varid
      external        nf_inq_varid
      integer(kind=4)         nf_inq_varname
      external        nf_inq_varname
      integer(kind=4)         nf_inq_vartype
      external        nf_inq_vartype
      integer(kind=4)         nf_inq_varndims
      external        nf_inq_varndims
      integer(kind=4)         nf_inq_vardimid
      external        nf_inq_vardimid
      integer(kind=4)         nf_inq_varnatts
      external        nf_inq_varnatts
      integer(kind=4)         nf_rename_var
      external        nf_rename_var
      integer(kind=4)         nf_copy_var
      external        nf_copy_var
      integer(kind=4)         nf_put_var_text
      external        nf_put_var_text
      integer(kind=4)         nf_get_var_text
      external        nf_get_var_text
      integer(kind=4)         nf_put_var_int1
      external        nf_put_var_int1
      integer(kind=4)         nf_get_var_int1
      external        nf_get_var_int1
      integer(kind=4)         nf_put_var_int2
      external        nf_put_var_int2
      integer(kind=4)         nf_get_var_int2
      external        nf_get_var_int2
      integer(kind=4)         nf_put_var_int
      external        nf_put_var_int
      integer(kind=4)         nf_get_var_int
      external        nf_get_var_int
      integer(kind=4)         nf_put_var_real
      external        nf_put_var_real
      integer(kind=4)         nf_get_var_real
      external        nf_get_var_real
      integer(kind=4)         nf_put_var_double
      external        nf_put_var_double
      integer(kind=4)         nf_get_var_double
      external        nf_get_var_double
      integer(kind=4)         nf_put_var1_text
      external        nf_put_var1_text
      integer(kind=4)         nf_get_var1_text
      external        nf_get_var1_text
      integer(kind=4)         nf_put_var1_int1
      external        nf_put_var1_int1
      integer(kind=4)         nf_get_var1_int1
      external        nf_get_var1_int1
      integer(kind=4)         nf_put_var1_int2
      external        nf_put_var1_int2
      integer(kind=4)         nf_get_var1_int2
      external        nf_get_var1_int2
      integer(kind=4)         nf_put_var1_int
      external        nf_put_var1_int
      integer(kind=4)         nf_get_var1_int
      external        nf_get_var1_int
      integer(kind=4)         nf_put_var1_real
      external        nf_put_var1_real
      integer(kind=4)         nf_get_var1_real
      external        nf_get_var1_real
      integer(kind=4)         nf_put_var1_double
      external        nf_put_var1_double
      integer(kind=4)         nf_get_var1_double
      external        nf_get_var1_double
      integer(kind=4)         nf_put_vara_text
      external        nf_put_vara_text
      integer(kind=4)         nf_get_vara_text
      external        nf_get_vara_text
      integer(kind=4)         nf_put_vara_int1
      external        nf_put_vara_int1
      integer(kind=4)         nf_get_vara_int1
      external        nf_get_vara_int1
      integer(kind=4)         nf_put_vara_int2
      external        nf_put_vara_int2
      integer(kind=4)         nf_get_vara_int2
      external        nf_get_vara_int2
      integer(kind=4)         nf_put_vara_int
      external        nf_put_vara_int
      integer(kind=4)         nf_get_vara_int
      external        nf_get_vara_int
      integer(kind=4)         nf_put_vara_real
      external        nf_put_vara_real
      integer(kind=4)         nf_get_vara_real
      external        nf_get_vara_real
      integer(kind=4)         nf_put_vara_double
      external        nf_put_vara_double
      integer(kind=4)         nf_get_vara_double
      external        nf_get_vara_double
      integer(kind=4)         nf_put_vars_text
      external        nf_put_vars_text
      integer(kind=4)         nf_get_vars_text
      external        nf_get_vars_text
      integer(kind=4)         nf_put_vars_int1
      external        nf_put_vars_int1
      integer(kind=4)         nf_get_vars_int1
      external        nf_get_vars_int1
      integer(kind=4)         nf_put_vars_int2
      external        nf_put_vars_int2
      integer(kind=4)         nf_get_vars_int2
      external        nf_get_vars_int2
      integer(kind=4)         nf_put_vars_int
      external        nf_put_vars_int
      integer(kind=4)         nf_get_vars_int
      external        nf_get_vars_int
      integer(kind=4)         nf_put_vars_real
      external        nf_put_vars_real
      integer(kind=4)         nf_get_vars_real
      external        nf_get_vars_real
      integer(kind=4)         nf_put_vars_double
      external        nf_put_vars_double
      integer(kind=4)         nf_get_vars_double
      external        nf_get_vars_double
      integer(kind=4)         nf_put_varm_text
      external        nf_put_varm_text
      integer(kind=4)         nf_get_varm_text
      external        nf_get_varm_text
      integer(kind=4)         nf_put_varm_int1
      external        nf_put_varm_int1
      integer(kind=4)         nf_get_varm_int1
      external        nf_get_varm_int1
      integer(kind=4)         nf_put_varm_int2
      external        nf_put_varm_int2
      integer(kind=4)         nf_get_varm_int2
      external        nf_get_varm_int2
      integer(kind=4)         nf_put_varm_int
      external        nf_put_varm_int
      integer(kind=4)         nf_get_varm_int
      external        nf_get_varm_int
      integer(kind=4)         nf_put_varm_real
      external        nf_put_varm_real
      integer(kind=4)         nf_get_varm_real
      external        nf_get_varm_real
      integer(kind=4)         nf_put_varm_double
      external        nf_put_varm_double
      integer(kind=4)         nf_get_varm_double
      external        nf_get_varm_double
      integer(kind=4)         nf_inquire_dimension
      external        nf_inquire_dimension
      integer(kind=4) nccre
      integer(kind=4) ncopn
      integer(kind=4) ncddef
      integer(kind=4) ncdid
      integer(kind=4) ncvdef
      integer(kind=4) ncvid
      integer(kind=4) nctlen
      integer(kind=4) ncsfil
      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil
      integer(kind=4) ncrdwr
      integer(kind=4) nccreat
      integer(kind=4) ncexcl
      integer(kind=4) ncindef
      integer(kind=4) ncnsync
      integer(kind=4) nchsync
      integer(kind=4) ncndirty
      integer(kind=4) nchdirty
      integer(kind=4) nclink
      integer(kind=4) ncnowrit
      integer(kind=4) ncwrite
      integer(kind=4) ncclob
      integer(kind=4) ncnoclob
      integer(kind=4) ncglobal
      integer(kind=4) ncfill
      integer(kind=4) ncnofill
      integer(kind=4) maxncop
      integer(kind=4) maxncdim
      integer(kind=4) maxncatt
      integer(kind=4) maxncvar
      integer(kind=4) maxncnam
      integer(kind=4) maxvdims
      integer(kind=4) ncnoerr
      integer(kind=4) ncebadid
      integer(kind=4) ncenfile
      integer(kind=4) nceexist
      integer(kind=4) nceinval
      integer(kind=4) nceperm
      integer(kind=4) ncenotin
      integer(kind=4) nceindef
      integer(kind=4) ncecoord
      integer(kind=4) ncemaxds
      integer(kind=4) ncename
      integer(kind=4) ncenoatt
      integer(kind=4) ncemaxat
      integer(kind=4) ncebadty
      integer(kind=4) ncebadd
      integer(kind=4) ncests
      integer(kind=4) nceunlim
      integer(kind=4) ncemaxvs
      integer(kind=4) ncenotvr
      integer(kind=4) nceglob
      integer(kind=4) ncenotnc
      integer(kind=4) ncfoobar
      integer(kind=4) ncsyserr
      integer(kind=4) ncfatal
      integer(kind=4) ncverbos
      integer(kind=4) ncentool
      integer(kind=4) ncbyte
      integer(kind=4) ncchar
      integer(kind=4) ncshort
      integer(kind=4) nclong
      integer(kind=4) ncfloat
      integer(kind=4) ncdouble
      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)
      parameter(ncrdwr = 1)
      parameter(nccreat = 2)
      parameter(ncexcl = 4)
      parameter(ncindef = 8)
      parameter(ncnsync = 16)
      parameter(nchsync = 32)
      parameter(ncndirty = 64)
      parameter(nchdirty = 128)
      parameter(ncfill = 0)
      parameter(ncnofill = 256)
      parameter(nclink = 32768)
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)
      integer(kind=4) ncunlim
      parameter(ncunlim = 0)
      parameter(ncglobal  = 0)
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)
      parameter(ncnoerr = nf_noerr)
      parameter(ncebadid = nf_ebadid)
      parameter(ncenfile = -31)
      parameter(nceexist = nf_eexist)
      parameter(nceinval = nf_einval)
      parameter(nceperm = nf_eperm)
      parameter(ncenotin = nf_enotindefine )
      parameter(nceindef = nf_eindefine)
      parameter(ncecoord = nf_einvalcoords)
      parameter(ncemaxds = nf_emaxdims)
      parameter(ncename = nf_enameinuse)
      parameter(ncenoatt = nf_enotatt)
      parameter(ncemaxat = nf_emaxatts)
      parameter(ncebadty = nf_ebadtype)
      parameter(ncebadd = nf_ebaddim)
      parameter(nceunlim = nf_eunlimpos)
      parameter(ncemaxvs = nf_emaxvars)
      parameter(ncenotvr = nf_enotvar)
      parameter(nceglob = nf_eglobal)
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)
      integer(kind=4) filbyte
      integer(kind=4) filchar
      integer(kind=4) filshort
      integer(kind=4) fillong
      real(kind=4) filfloat
      doubleprecision fildoub
      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)
      integer(kind=8) sz_init, sz_read_acc, sz_write_acc
      common /sz_acc/ sz_init, sz_read_acc, sz_write_acc

 CONTAINS

   

 END MODULE kk_netcdf_common
