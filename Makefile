.PHONY: compile
compile:
	rebar get-deps compile escriptize

.PHONY: clean
clean:
	rm ebin/* *.log

#-------------------------------------------------------------------------------
OTP_PLT   = .epm_otp.plt
COMBO_PLT = .epm_combo.plt
PLT_LIBS0 = ebin $(wildcard deps/*/ebin)
PLT_LIBS  = $(subst deps/riak_pb/ebin,,$(PLT_LIBS0))

DIALYZER_APPS = epm
DIALYZER_APPS_PATHS = $(wildcard ebin/*.beam)
#$(addsuffix /ebin, $(addprefix apps/, $(DIALYZER_APPS)))

.PHONY: check_plt
check_plt: $(COMBO_PLT)
	dialyzer --check_plt --plt $(COMBO_PLT) $(PLT_LIBS)

.PHONY: build_sysplt
build_sysplt: $(OTP_PLT)

$(OTP_PLT):
	dialyzer --output_plt $(OTP_PLT) --build_plt \
		--apps erts kernel stdlib inets sasl ssl public_key crypto mnesia

.PHONY: build_plt
build_plt: compile build_sysplt $(COMBO_PLT)

$(COMBO_PLT):
	dialyzer --plt $(OTP_PLT) --output_plt $(COMBO_PLT) --add_to_plt $(PLT_LIBS)

.PHONY: dialyze
dialyze: compile check_plt
	dialyzer -Wno_return --fullpath --plt $(COMBO_PLT) $(DIALYZER_APPS_PATHS) \
	    | fgrep -v -f ./dialyzer.ignore-warnings | tee dialyzer.log -

.PHONY: cleanplt
cleanplt:
	rm $(COMBO_PLT)
