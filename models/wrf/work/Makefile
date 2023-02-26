# Makefile created by mkmf $Id$ 

include /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/build_templates/mkmf.template
.DEFAULT:
	-touch $@
all: fixsys filter
fixsys: 
	@ (cd /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities; ./fixsystem $(FC) )
obs_def_utilities_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk//observations/forward_operators/obs_def_utilities_mod.f90 types_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk//observations/forward_operators/obs_def_utilities_mod.f90
model_check_utilities_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/model_check_utilities_mod.f90 types_mod.o utilities_mod.o location_mod.o obs_kind_mod.o ensemble_manager_mod.o model_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/model_check_utilities_mod.f90
filter_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/filter_mod.f90 types_mod.o options_mod.o obs_sequence_mod.o obs_def_mod.o obs_def_utilities_mod.o time_manager_mod.o utilities_mod.o assim_model_mod.o assim_tools_mod.o obs_model_mod.o ensemble_manager_mod.o adaptive_inflate_mod.o mpi_utilities_mod.o smoother_mod.o random_seq_mod.o state_vector_io_mod.o io_filenames_mod.o direct_netcdf_mod.o state_structure_mod.o forward_operator_mod.o quality_control_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/filter_mod.f90
quality_control_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/quality_control_mod.f90 types_mod.o utilities_mod.o location_mod.o obs_sequence_mod.o obs_def_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/quality_control_mod.f90
mpi_utilities_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/mpi_utilities_mod.f90 types_mod.o utilities_mod.o time_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/mpi_utilities_mod.f90
utilities_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/utilities_mod.f90 types_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/utilities_mod.f90
dart_time_io_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/dart_time_io_mod.f90 types_mod.o time_manager_mod.o utilities_mod.o netcdf_utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/dart_time_io_mod.f90
sort_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/sort_mod.f90 types_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/sort_mod.f90
module_map_utils.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/module_map_utils.f90 types_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/module_map_utils.f90
f2kcli.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/f2kcli.f90
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/f2kcli.f90
SW_test.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/SW_test.f90 common.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/SW_test.f90
module_timediff.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_timediff.f90 types_mod.o time_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_timediff.f90
time_manager_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/time_manager_mod.f90 types_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/time_manager_mod.f90
obs_sequence_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_sequence_mod.f90 types_mod.o location_mod.o obs_def_mod.o obs_kind_mod.o time_manager_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_sequence_mod.f90
state_vector_io_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_vector_io_mod.f90 adaptive_inflate_mod.o direct_netcdf_mod.o types_mod.o mpi_utilities_mod.o ensemble_manager_mod.o utilities_mod.o time_manager_mod.o io_filenames_mod.o model_mod.o state_structure_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_vector_io_mod.f90
cov_cutoff_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/cov_cutoff_mod.f90 types_mod.o utilities_mod.o location_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/cov_cutoff_mod.f90
forward_operator_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/forward_operator_mod.f90 types_mod.o time_manager_mod.o utilities_mod.o mpi_utilities_mod.o obs_sequence_mod.o obs_def_mod.o obs_kind_mod.o ensemble_manager_mod.o distributed_state_mod.o quality_control_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/forward_operator_mod.f90
obs_impact_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/obs_impact_mod.f90 types_mod.o utilities_mod.o obs_kind_mod.o parse_args_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/obs_impact_mod.f90
reg_factor_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/reg_factor_mod.f90 types_mod.o utilities_mod.o time_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/reg_factor_mod.f90
common.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common.f90
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common.f90
random_seq_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/random_seq_mod.f90 types_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/random_seq_mod.f90
no_cray_win_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/no_cray_win_mod.f90 mpi_utilities_mod.o types_mod.o ensemble_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/no_cray_win_mod.f90
wrf_data_module.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/wrf_data_module.f90 types_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/wrf_data_module.f90
module_couple_uv.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_couple_uv.f90 types_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_couple_uv.f90
module_netcdf_interface.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_netcdf_interface.f90 types_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_netcdf_interface.f90
assim_tools_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_tools_mod.f90 types_mod.o options_mod.o utilities_mod.o sort_mod.o random_seq_mod.o obs_sequence_mod.o obs_def_mod.o obs_kind_mod.o cov_cutoff_mod.o reg_factor_mod.o obs_impact_mod.o sampling_error_correction_mod.o location_mod.o ensemble_manager_mod.o mpi_utilities_mod.o adaptive_inflate_mod.o time_manager_mod.o assim_model_mod.o distributed_state_mod.o quality_control_mod.o common.o common_cdf.o prob.o SW_test.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_tools_mod.f90
netcdf_utilities_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/netcdf_utilities_mod.f90 types_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/netcdf_utilities_mod.f90
io_filenames_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/io_filenames_mod.f90 types_mod.o utilities_mod.o time_manager_mod.o mpi_utilities_mod.o state_structure_mod.o ensemble_manager_mod.o netcdf_utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/io_filenames_mod.f90
direct_netcdf_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/direct_netcdf_mod.f90 types_mod.o options_mod.o ensemble_manager_mod.o time_manager_mod.o utilities_mod.o netcdf_utilities_mod.o mpi_utilities_mod.o state_structure_mod.o io_filenames_mod.o assim_model_mod.o model_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/direct_netcdf_mod.f90
obs_kind_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_kind_mod.f90 types_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_kind_mod.f90
model_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/model_mod.f90 types_mod.o time_manager_mod.o location_mod.o utilities_mod.o netcdf_utilities_mod.o mpi_utilities_mod.o random_seq_mod.o obs_kind_mod.o ensemble_manager_mod.o sort_mod.o distributed_state_mod.o default_model_mod.o state_structure_mod.o module_map_utils.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/model_mod.f90
location_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/threed_sphere/location_mod.f90 types_mod.o utilities_mod.o random_seq_mod.o obs_kind_mod.o mpi_utilities_mod.o ensemble_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/threed_sphere/location_mod.f90
types_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/types_mod.f90
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/types_mod.f90
filter.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/programs/filter/filter.f90 mpi_utilities_mod.o filter_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/programs/filter/filter.f90
distributed_state_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/distributed_state_mod.f90 mpi_utilities_mod.o types_mod.o ensemble_manager_mod.o no_cray_win_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/distributed_state_mod.f90
ensemble_manager_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/ensemble_manager_mod.f90 types_mod.o utilities_mod.o time_manager_mod.o mpi_utilities_mod.o sort_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/ensemble_manager_mod.f90
prob.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/prob.f90
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/prob.f90
location_io_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//location_io_mod.f90 types_mod.o netcdf_utilities_mod.o location_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//location_io_mod.f90
quad_utils_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//quad_utils_mod.f90 types_mod.o location_mod.o utilities_mod.o options_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//quad_utils_mod.f90
common_cdf.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common_cdf.f90
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common_cdf.f90
schedule_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/schedule_mod.f90 types_mod.o utilities_mod.o time_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/schedule_mod.f90
options_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/options_mod.f90 types_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/options_mod.f90
default_model_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//default_model_mod.f90 types_mod.o time_manager_mod.o location_mod.o utilities_mod.o netcdf_utilities_mod.o ensemble_manager_mod.o dart_time_io_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//default_model_mod.f90
obs_def_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/observations/forward_operators/obs_def_mod.f90 types_mod.o utilities_mod.o location_mod.o assim_model_mod.o obs_kind_mod.o ensemble_manager_mod.o obs_def_utilities_mod.o time_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/observations/forward_operators/obs_def_mod.f90
obs_model_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/obs_model_mod.f90 utilities_mod.o assim_model_mod.o state_vector_io_mod.o obs_sequence_mod.o obs_def_mod.o time_manager_mod.o ensemble_manager_mod.o mpi_utilities_mod.o io_filenames_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/obs_model_mod.f90
sampling_error_correction_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90 types_mod.o utilities_mod.o netcdf_utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90
assim_model_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_model_mod.f90 types_mod.o time_manager_mod.o ensemble_manager_mod.o model_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_model_mod.f90
adaptive_inflate_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90 types_mod.o time_manager_mod.o utilities_mod.o random_seq_mod.o ensemble_manager_mod.o mpi_utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90
smoother_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/smoother_mod.f90 types_mod.o mpi_utilities_mod.o utilities_mod.o ensemble_manager_mod.o time_manager_mod.o options_mod.o assim_tools_mod.o obs_sequence_mod.o adaptive_inflate_mod.o io_filenames_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/smoother_mod.f90
assert_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/assert_mod.f90
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/assert_mod.f90
default_location_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//default_location_mod.f90 types_mod.o ensemble_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//default_location_mod.f90
parse_args_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/parse_args_mod.f90 utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/parse_args_mod.f90
state_structure_mod.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_structure_mod.f90 utilities_mod.o obs_kind_mod.o types_mod.o netcdf_utilities_mod.o sort_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_structure_mod.f90
test_interpolate_threed_sphere.o: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/test_interpolate_threed_sphere.f90 types_mod.o utilities_mod.o netcdf_utilities_mod.o location_mod.o obs_kind_mod.o ensemble_manager_mod.o model_check_utilities_mod.o model_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	/glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/test_interpolate_threed_sphere.f90
./location_io_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//location_io_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//location_io_mod.f90 .
./types_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/types_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/types_mod.f90 .
./assert_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/assert_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/assert_mod.f90 .
./reg_factor_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/reg_factor_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/reg_factor_mod.f90 .
./time_manager_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/time_manager_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/time_manager_mod.f90 .
./prob.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/prob.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/prob.f90 .
./common.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common.f90 .
./wrf_data_module.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/wrf_data_module.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/wrf_data_module.f90 .
./common_cdf.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common_cdf.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common_cdf.f90 .
./SW_test.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/SW_test.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/SW_test.f90 .
./netcdf_utilities_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/netcdf_utilities_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/netcdf_utilities_mod.f90 .
./options_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/options_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/options_mod.f90 .
./quad_utils_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//quad_utils_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//quad_utils_mod.f90 .
./cov_cutoff_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/cov_cutoff_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/cov_cutoff_mod.f90 .
./schedule_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/schedule_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/schedule_mod.f90 .
./module_timediff.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_timediff.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_timediff.f90 .
./assim_model_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_model_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_model_mod.f90 .
./forward_operator_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/forward_operator_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/forward_operator_mod.f90 .
./filter.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/programs/filter/filter.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/programs/filter/filter.f90 .
./parse_args_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/parse_args_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/parse_args_mod.f90 .
./test_interpolate_threed_sphere.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/test_interpolate_threed_sphere.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/test_interpolate_threed_sphere.f90 .
./smoother_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/smoother_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/smoother_mod.f90 .
./f2kcli.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/f2kcli.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/f2kcli.f90 .
./model_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/model_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/model_mod.f90 .
./state_structure_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_structure_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_structure_mod.f90 .
./module_netcdf_interface.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_netcdf_interface.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_netcdf_interface.f90 .
./state_vector_io_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_vector_io_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_vector_io_mod.f90 .
./io_filenames_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/io_filenames_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/io_filenames_mod.f90 .
./utilities_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/utilities_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/utilities_mod.f90 .
./sampling_error_correction_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90 .
./obs_def_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/observations/forward_operators/obs_def_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/observations/forward_operators/obs_def_mod.f90 .
./ensemble_manager_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/ensemble_manager_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/ensemble_manager_mod.f90 .
./quality_control_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/quality_control_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/quality_control_mod.f90 .
./obs_sequence_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_sequence_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_sequence_mod.f90 .
./mpi_utilities_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/mpi_utilities_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/mpi_utilities_mod.f90 .
./random_seq_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/random_seq_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/random_seq_mod.f90 .
./default_location_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//default_location_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//default_location_mod.f90 .
./adaptive_inflate_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90 .
./model_check_utilities_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/model_check_utilities_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/model_check_utilities_mod.f90 .
./obs_model_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/obs_model_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/obs_model_mod.f90 .
./filter_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/filter_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/filter_mod.f90 .
./module_map_utils.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/module_map_utils.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/module_map_utils.f90 .
./no_cray_win_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/no_cray_win_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/no_cray_win_mod.f90 .
./direct_netcdf_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/direct_netcdf_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/direct_netcdf_mod.f90 .
./assim_tools_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_tools_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_tools_mod.f90 .
./obs_def_utilities_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk//observations/forward_operators/obs_def_utilities_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk//observations/forward_operators/obs_def_utilities_mod.f90 .
./module_couple_uv.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_couple_uv.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_couple_uv.f90 .
./distributed_state_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/distributed_state_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/distributed_state_mod.f90 .
./default_model_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//default_model_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//default_model_mod.f90 .
./sort_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/sort_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/sort_mod.f90 .
./dart_time_io_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/dart_time_io_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/dart_time_io_mod.f90 .
./obs_kind_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_kind_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_kind_mod.f90 .
./obs_impact_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/obs_impact_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/obs_impact_mod.f90 .
./location_mod.f90: /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/threed_sphere/location_mod.f90
	cp /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/threed_sphere/location_mod.f90 .
SRC = /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk//observations/forward_operators/obs_def_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/model_check_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/filter_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/quality_control_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/mpi_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/dart_time_io_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/sort_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/module_map_utils.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/f2kcli.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/SW_test.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_timediff.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/time_manager_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_sequence_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_vector_io_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/cov_cutoff_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/forward_operator_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/obs_impact_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/reg_factor_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/random_seq_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/no_cray_win_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/wrf_data_module.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_couple_uv.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_netcdf_interface.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_tools_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/netcdf_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/io_filenames_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/direct_netcdf_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_kind_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/threed_sphere/location_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/types_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/programs/filter/filter.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/distributed_state_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/ensemble_manager_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/prob.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//location_io_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//quad_utils_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common_cdf.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/schedule_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/options_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//default_model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/observations/forward_operators/obs_def_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/obs_model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/smoother_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/assert_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//default_location_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/parse_args_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_structure_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/test_interpolate_threed_sphere.f90
OBJ = obs_def_utilities_mod.o model_check_utilities_mod.o filter_mod.o quality_control_mod.o mpi_utilities_mod.o utilities_mod.o dart_time_io_mod.o sort_mod.o module_map_utils.o f2kcli.o SW_test.o module_timediff.o time_manager_mod.o obs_sequence_mod.o state_vector_io_mod.o cov_cutoff_mod.o forward_operator_mod.o obs_impact_mod.o reg_factor_mod.o common.o random_seq_mod.o no_cray_win_mod.o wrf_data_module.o module_couple_uv.o module_netcdf_interface.o assim_tools_mod.o netcdf_utilities_mod.o io_filenames_mod.o direct_netcdf_mod.o obs_kind_mod.o model_mod.o location_mod.o types_mod.o filter.o distributed_state_mod.o ensemble_manager_mod.o prob.o location_io_mod.o quad_utils_mod.o common_cdf.o schedule_mod.o options_mod.o default_model_mod.o obs_def_mod.o obs_model_mod.o sampling_error_correction_mod.o assim_model_mod.o adaptive_inflate_mod.o smoother_mod.o assert_mod.o default_location_mod.o parse_args_mod.o state_structure_mod.o test_interpolate_threed_sphere.o
OFF = /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//location_io_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/types_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/assert_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/reg_factor_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/time_manager_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/prob.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/wrf_data_module.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/common_cdf.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/SW_test.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/netcdf_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/options_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//quad_utils_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/cov_cutoff_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/schedule_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_timediff.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/forward_operator_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/programs/filter/filter.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/parse_args_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/test_interpolate_threed_sphere.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/smoother_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_DART_utilities/f2kcli.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_structure_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_netcdf_interface.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/state_vector_io_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/io_filenames_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/observations/forward_operators/obs_def_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/ensemble_manager_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/quality_control_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_sequence_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/mpi_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/random_seq_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/utilities//default_location_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/model_mod_tools/model_check_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/obs_model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/filter_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/module_map_utils.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/no_cray_win_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/direct_netcdf_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/assimilation/assim_tools_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk//observations/forward_operators/obs_def_utilities_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/wrf/WRF_BC/module_couple_uv.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/distributed_state_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/models/utilities//default_model_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/sort_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/dart_time_io_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/observations/obs_kind_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/obs_impact_mod.f90 /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/threed_sphere/location_mod.f90
clean: neat
	-rm -f .cppdefs $(OBJ) filter
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
filter: $(OBJ)
	$(MPILD) $(OBJ) -o filter  $(LDFLAGS)
