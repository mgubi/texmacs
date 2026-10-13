(%fl:1+ #3=#fn("7000r1|aw;" #() 1+) %fl:1-
	#4=#fn("7000r1|ax;" #() 1-) %fl:1arg-lambda? #5=#fn("8000r1|F16Z02|Mc0<16P02|NF16H02e1|31F16=02e2e1|31a42;" #(lambda
  %fl:cadr %fl:length=) 1arg-lambda?)
	%fl:<= #6=#fn("7000r2}|X17B02e0|3116802e0}31@;" #(%fl:nan?) <=) %fl:>
	#7=#fn("7000r2}|X;" #() >) %fl:>= #8=#fn("7000r2|}X17B02e0|3116802e0}31@;" #(%fl:nan?) >=)
	%fl:__init_globals #9=#fn("7000r0e0c1<17B02e0c2<17802e0c3<6>0c4k52c6k75;0c8k52c9k72e:k;2e<k=2e>k?;" #(*os-name*
  win32 win64 windows "\\" *directory-separator* "\x0d\n" *linefeed* "/" "\n"
  *stdout* *output-stream* *stdin* *input-stream* *stderr* *error-stream*) __init_globals)
	%fl:__script #10=#fn("7000r1c0qc1t;" #(#fn("7000r0e0~41;" #(%fl:load))
					       #fn("7000r1e0|312c1a41;" #(%fl:top-level-exception-handler
  #fn(exit)))) __script)
	%fl:__start #11=#fn("8000r1e0302|NF6G0|Nk12^k22e3e4|31315E0|k12]k22e5e6312e7302c8`41;" #(%fl:__init_globals
  *argv* *interactive* %fl:__script %fl:cadr %fl:princ *banner* %fl:repl #fn(exit)) __start)
	%fl:abs #12=#fn("7000r1|`X650|y;|;" #() abs) %fl:any
	#13=#fn("8000r2}F16D02|}M3117:02e0|}N42;" #(%fl:any) any)
	%fl:argc-error #14=#fn("<000r2e0c1|c2}}aW670c3540c445;" #(%fl:error "compile error: "
								  " expects "
								  " argument."
								  " arguments.") argc-error)
	%fl:array #fn(array) %fl:array?
	#15=#fn("8000r1|H17<02c0c1|3141;" #(#fn("7000r1|F16802|Mc0<;" #(array))
					    #fn(typeof)) array?)
	%fl:assoc #16=#fn("8000r2}?640^;e0}31|>650}M;e1|}N42;" #(%fl:caar
								 %fl:assoc) assoc)
	%fl:assv #17=#fn("8000r2}?640^;e0}31|=650}M;e1|}N42;" #(%fl:caar
								%fl:assv) assv)
	%fl:bcode:cdepth #18=#fn(":000r2|b3e0|b3[}32\\;" #(%fl:min) bcode:cdepth)
	%fl:bcode:code #19=#fn("7000r1|`[;" #() bcode:code) %fl:bcode:ctable
	#20=#fn("7000r1|a[;" #() bcode:ctable) %fl:bcode:indexfor #21=#fn("9000r2c0qe1|31e2|3142;" #(#fn(":000r2c0|\x7f32690c1|\x7f42;c2|\x7f}332}~b2}aw\\2;" #(#fn(has?)
  #fn(get) #fn(put!))) %fl:bcode:ctable %fl:bcode:nconst) bcode:indexfor)
	%fl:bcode:nconst #22=#fn("7000r1|b2[;" #() bcode:nconst) %fl:bq-bracket
	#23=#fn("<000r2|?6=0c0e1|}32L2;|Mc2ÇR0}`W680c0|NK;c0c3c4e1|N}ax32L3L2;|Mc5ÇY0}`W6<0c6e7|31L2;c0c0c8e1e7|31}ax32L3L2;|Mc9ÇU0}`W680e7|41;c0c0c:e1e7|31}ax32L3L2;c0e1|}32L2;" #(#.list
  %fl:bq-process unquote #.cons 'unquote unquote-splicing copy-list %fl:cadr 'unquote-splicing
  unquote-nsplicing 'unquote-nsplicing) bq-bracket)
	%fl:bq-bracket1 #24=#fn(";000r2|F16802|Mc0<6N0}`W680e1|41;c2c3e4|N}ax32L3;e4|}42;" #(unquote
  %fl:cadr #.cons 'unquote %fl:bq-process) bq-bracket1)
	%fl:bq-process #25=#fn(";000r2|C680c0|L2;|H6A0c1e2e3|31}3241;|?640|;|Mc4ÇE0c5c6e2e7|31}aw32L3;|Mc8ÇZ0}`W16:02e9|b232680e7|41;c:c;e2|N}ax32L3;e<e=|327E0c>qe?|31c@cAq|3242;cBqcC31|_42;" #(quote
  #fn("8000r1|Mc0Ç80c1|NK;c2c1|L3;" #(#.list #.vector #.apply)) %fl:bq-process
  %fl:vector->list quasiquote #.list 'quasiquote %fl:cadr unquote %fl:length=
  #.cons 'unquote %fl:any %fl:splice-form? #fn(":000r2|Ö70c0}K;}NÖ?0c1}Me2|\x7f32L3;c3c4}Ke2|\x7f32L142;" #(#.list
  #.cons %fl:bq-process #fn(nconc) #fn(list*))) %fl:lastcdr #fn(map)
  #fn("8000r1e0|\x7f42;" #(%fl:bq-bracket1))
  #fn("6000r1c0qm02|;" #(#fn(">000r2|Ö;0c0e1}31K;|F6s0|Mc2Ç[0c0e3}i11`W670|N5E0c4c5L2e6|Ni11ax32L232K;~|Ne7|Mi1132}K42;c0e1e6|i1132}K31K;" #(nconc
  %fl:reverse! unquote %fl:nreconc #.list 'unquote %fl:bq-process
  %fl:bq-bracket)))) #{#<unspecified>}#) bq-process)
	%fl:builtin->instruction #26=#fn("9000r1c0~|^43;" #(#fn(get)) #(#table(#.equal? equal?  #.* *  #.car car  #.apply apply  #.aref aref  #.- -  #.boolean? boolean?  #.builtin? builtin?  #.null? null?  #.eqv? eqv?  #.function? function?  #.bound? bound?  #.cdr cdr  #.list list  #.set-car! set-car!  #.cons cons  #.atom? atom?  #.set-cdr! set-cdr!  #.symbol? symbol?  #.eq? eq?  #.vector vector  #.not not  #.pair? pair?  #.number? number?  #.div0 div0  #.aset! aset!  #.+ +  #.= =  #.compare compare  #.vector? vector?  #./ /  #.< <  #.fixnum? fixnum?)
  ()))
	%fl:builtin-argc-ok? #27=#fn(":000r2c0qc1e2|^3341;" #(#fn("7000r1|@17602\x7f|W16\\02\x7f`W16@02~c0<17702~c1<@16A02\x7fb2X16702~c2<@;" #(#.-
  #./ #.apply)) #fn(get) arg-counts) builtin-argc-ok?)
	%fl:byte #fn(byte) %fl:caaaar
	#28=#fn("6000r1|MMMM;" #() caaaar) %fl:caaadr #29=#fn("6000r1|ÑMM;" #() caaadr)
	%fl:caaar #30=#fn("6000r1|MMM;" #() caaar) %fl:caadar
	#31=#fn("6000r1|MÑM;" #() caadar) %fl:caaddr #32=#fn("6000r1|NÑM;" #() caaddr)
	%fl:caadr #33=#fn("6000r1|ÑM;" #() caadr) %fl:caar
	#34=#fn("6000r1|MM;" #() caar) %fl:cadaar #35=#fn("6000r1|MMÑ;" #() cadaar)
	%fl:cadadr #36=#fn("6000r1|ÑÑ;" #() cadadr) %fl:cadar
	#37=#fn("6000r1|MÑ;" #() cadar) %fl:caddar #38=#fn("6000r1|MNÑ;" #() caddar)
	%fl:cadddr #39=#fn("6000r1|NNÑ;" #() cadddr) %fl:caddr
	#40=#fn("6000r1|NÑ;" #() caddr) %fl:cadr #41=#fn("6000r1|Ñ;" #() cadr)
	%fl:call-with-values #42=#fn("7000r2c0q|3041;" #(#fn("7000r1|F16902i10|M<680\x7f|Nv2;\x7f|41;" #())) #2=#((*values*)
  ()))
	%fl:cdaaar #43=#fn("6000r1|MMMN;" #() cdaaar) %fl:cdaadr
	#44=#fn("6000r1|ÑMN;" #() cdaadr) %fl:cdaar #45=#fn("6000r1|MMN;" #() cdaar)
	%fl:cdadar #46=#fn("6000r1|MÑN;" #() cdadar) %fl:cdaddr
	#47=#fn("6000r1|NÑN;" #() cdaddr) %fl:cdadr #48=#fn("6000r1|ÑN;" #() cdadr)
	%fl:cdar #49=#fn("6000r1|MN;" #() cdar) %fl:cddaar
	#50=#fn("6000r1|MMNN;" #() cddaar) %fl:cddadr #51=#fn("6000r1|ÑNN;" #() cddadr)
	%fl:cddar #52=#fn("6000r1|MNN;" #() cddar) %fl:cdddar
	#53=#fn("6000r1|MNNN;" #() cdddar) %fl:cddddr #54=#fn("6000r1|NNNN;" #() cddddr)
	%fl:cdddr #55=#fn("6000r1|NNN;" #() cdddr) %fl:cddr
	#56=#fn("6000r1|NN;" #() cddr) %fl:char? #57=#fn("7000r1c0|31c1<;" #(#fn(typeof)
  wchar) char?)
	%fl:closure? #58=#fn("7000r1|J16602|G@;" #() closure?) %fl:compile
	#59=#fn("8000r1e0_|42;" #(%fl:compile-f) compile) %fl:compile-and #60=#fn("<000r4e0|}g2g3]c146;" #(%fl:compile-short-circuit
  brf) compile-and)
	%fl:compile-app #61=#fn("7000r4c0qg3M41;" #(#fn("9000r1c0q|C16:02e1|\x7f32@6:0e2|31530|41;" #(#fn("9000r1c0q~C16X02e1~i1132@16J02|E16C02c2|3116902c3|31G6:0c3|31530~41;" #(#fn("<000r1|c0<17702|c1<16=02e2i23b332@6R0e3i20i21i22|c0Ç70c4540c5i23NK44;e6i23Nc7326S0e3i20i21^|342c8qe9i20i21i23N3341;c:q|G16J02e;|c<i23N313216802e=|3141;" #(#.<
  #.= %fl:length= %fl:compile-in nary< nary= %fl:length> 255 #fn(":000r1e0i30i32670c1540c2|43;" #(%fl:emit
  tcall.l call.l)) %fl:compile-arglist #fn(";000r1~c0<16c02i10c0<16X02e1~i3132@16J02c2c031e3>16<02e4i33b2326O0e5i30i31^e3i3331342e6i30c042;|7A0e5i30i31^~34540c72c8qe9i30i31i33N3341;" #(cadr
  %fl:in-env? #fn(top-level-value) %fl:cadr %fl:length= %fl:compile-in %fl:emit
  #{#<unspecified>}# #fn("=000r1~6H0e0i40i41i42i43i10~|47;e1i40i42670c2540c3|43;" #(%fl:compile-builtin-call
  %fl:emit tcall call)) %fl:compile-arglist))
  %fl:builtin-argc-ok? #fn(length) %fl:builtin->instruction)) %fl:in-env? #fn(constant?)
  #fn(top-level-value))) %fl:in-env? resolve-global))) compile-app)
	%fl:compile-arglist #62=#fn("8000r3e0c1qg2322c2g241;" #(%fl:for-each #fn(":000r1e0~\x7f^|44;" #(%fl:compile-in))
								#fn(length)) compile-arglist)
	%fl:compile-begin #63=#fn(":000r4g3?6?0e0|}g2e13044;g3N?6>0e0|}g2g3M44;e0|}^g3M342e2|c3322e4|}g2g3N44;" #(%fl:compile-in
  %fl:void %fl:emit pop %fl:compile-begin) compile-begin)
	%fl:compile-builtin-call #64=#fn(":000r7c0qc1e2g4^3341;" #(#fn("8000r1|16=02e0i03N|32@6=0e1i05|32540c22c3qi0541;" #(%fl:length=
  %fl:argc-error #{#<unspecified>}# #fn(":000r1|c0ÇR0i16`W6<0e1i10c242;e1i10i15i1643;|c3Çe0i16`W6<0e1i10c442;i16b2W6<0e1i10c542;e1i10i15i1643;|c6Çv0i16`W6;0e7i15a42;i16aW6<0e1i10c842;i16b2W6<0e1i10c942;e1i10i15i1643;|c:ÇR0i16`W6<0e1i10c;42;e1i10i15i1643;|c<ÇQ0i16`W6;0e7i15a42;e1i10i15i1643;|c=ÇT0i16`W6>0e1i10c>c?43;e1i10i15i1643;|c@Ç]0i16b2X6<0e7i15b242;e1i10i12670cA540c@i1643;e1i10i1542;" #(list
  %fl:emit loadnil + load0 add2 - %fl:argc-error neg sub2 * load1 / vector
  loadv #() apply tapply)))) #fn(get) arg-counts) compile-builtin-call)
	%fl:compile-f #65=#fn("8000r2e0c1qc242;" #(%fl:call-with-values
						   #fn("8000r0e0~\x7f42;" #(%fl:compile-f-))
						   #fn("6000r2|;" #())) compile-f)
	%fl:compile-f- #66=#fn("8000r2c0qc1c142;" #(#fn(">000r2c0qm02c1qm12c2qe330e4\x7f31e5e4\x7f3131e6e4\x7f3131e7c8e4\x7f3132e5\x7f31i10Ç70c9570e5\x7f3146;" #(#fn("9000r1c0qe1|31F6N0e2|31F6=0c3e1|31K570e4|31560e53041;" #(#fn("8000r1c0qe1|3141;" #(#fn(":000r1|Ö40~;c0c1|~i4034c2c3|32K;" #(#fn(list*)
  lambda #fn(map) #fn("6000r1e040;" #(%fl:void))))
  %fl:get-defined-vars)) %fl:cddr %fl:cdddr begin %fl:caddr %fl:void) lambda-body)
  #fn("7000r1e0|31i20Ç80e1|41;~|41;" #(%fl:lastcdr %fl:caddr) lam:body)
  #fn("9000r6c0q}?660`570c1}3141;" #(#fn("9000r1c0q|c1i0431x41;" #(#fn("9000r1c0qe1e2i143241;" #(#fn("F000r1i24á©0|ÖO0e0i20c1~i22Ö80i10560i10y345s0e2i20e3c4c5c4c6|32e7c8|31313331322e0i20c9~c8|31i22Ö80i10560i10y352e:i20i40i24i23~35540c;2e<i10c=326L0e0i20i22Ö70c>540c?i10335]0i22áA0e0i20c@i10335H0i24ÖA0e0i20cAi1033530^2eBi20i23i40K]i31i4131342e0i20cC322eD16?02eEi4131i50<@6W0e2i20cFcGcHeIi4131eJeKi41313133K32540c;2eLcMeNeOi203131ePi2031i2533i20b3[42;" #(%fl:emit
  optargs %fl:bcode:indexfor %fl:make-perfect-hash-table
  #fn(map) #.cons #.car %fl:iota #fn(length) keyargs
  %fl:emit-optional-arg-inits #{#<unspecified>}# %fl:> 255 largc lvargc vargc
  argc %fl:compile-in ret *keep-source* %fl:lastcdr %source #fn(list*) lambda
  %fl:cadr %fl:proper-part %fl:cddr %fl:values #fn(function)
  %fl:encode-byte-code %fl:bcode:code %fl:const-to-idx-vec)) %fl:filter
  %fl:keyword-arg?)) #fn(length))) #fn(length)))
  %fl:make-code-emitter %fl:cadr %fl:lastcdr %fl:lambda-vars %fl:filter #.pair?
  lambda)) #{#<unspecified>}#) #0=#(%defines-processed% ()))
	%fl:compile-for #67=#fn(":000r5e0g4316X0e1|}^g2342e1|}^g3342e1|}^g4342e2|c342;e4c541;" #(%fl:1arg-lambda?
  %fl:compile-in %fl:emit for %fl:error "for: third form must be a 1-argument lambda") compile-for)
	%fl:compile-if #68=#fn("<000r4c0qe1|31e1|31e2g331e3g331e4g331F6;0e5g331560e63045;" #(#fn(";000r5g2]Ç>0e0~\x7fi02g344;g2^Ç>0e0~\x7fi02g444;e0~\x7f^g2342e1~c2|332e0~\x7fi02g3342i026<0e1~c3325:0e1~c4}332e5~|322e0~\x7fi02g4342e5~}42;" #(%fl:compile-in
  %fl:emit brf ret jmp %fl:mark-label)) %fl:make-label %fl:cadr %fl:caddr
  %fl:cdddr %fl:cadddr %fl:void) compile-if)
	%fl:compile-in #69=#fn(";000r4g3C6=0e0|}g3c144;g3?6Ø0g3`Ç:0e2|c342;g3aÇ:0e2|c442;g3]Ç:0e2|c542;g3^Ç:0e2|c642;g3_Ç:0e2|c742;e8g3316<0e2|c9g343;c:g3316C0e;|}g2c<c=31L144;e2|c>g343;g3MC@17W02c?g3Me@32@16H02eAg3M31E17;02eBg3M}326=0eC|}g2g344;cDqc?g3Me@32@16:02eEg3}3241;" #(%fl:compile-sym
  #(loada loadc loadg) %fl:emit load0 load1 loadt loadf loadnil %fl:fits-i8
  loadi8 #fn(eof-object?) %fl:compile-in #fn(top-level-value) eof-object loadv
  #fn(memq) special-forms resolve-global %fl:in-env? %fl:compile-app #fn(":000r1|6=0e0~\x7fi02|44;c1qi03M41;" #(%fl:compile-in
  #fn("=000r1|c0Çf0e1e2i1331316G0e3i10i11i12e2i133144;e4i10c5e2i133143;|c6ÇC0e7i10i11i12i1344;|c8ÇD0e9i10i11i12i13N44;|c:Ç@0e;i10i11i1343;|c<Ç=0e=c>qc?q42;|c@ÇD0eAi10i11i12i13N44;|cBÇD0eCi10i11i12i13N44;|cDÇN0eEi10i11e2i1331c8eFi1331K44;|cGÇR0eHi10i11e2i1331eIi1331eJi133145;|cKÇO0e3i10i11]e2i1331342e4i10cL42;|cMÇm0e3i10i11^eIi1331342e2i1331C17902eNcO312ePi10i11e2i1331cQ44;|cRÇG0e3i10i11i12eSi133144;|cTÇÄ0e3i10i11^c<_e2i1331L3342eUeIi133131660^580eNcV312e3i10i11^eIi1331342e4i10cT42;eWi10i11i12i1344;" #(quote
  %fl:self-evaluating? %fl:cadr %fl:compile-in %fl:emit loadv if %fl:compile-if
  begin %fl:compile-begin prog1 %fl:compile-prog1 lambda %fl:call-with-values
  #fn("8000r0e0i21i2342;" #(%fl:compile-f-))
  #fn("9000r2e0i20c1|332e2i20}322}e3i2131X6<0e0i20c442;c5;" #(%fl:emit loadv
							      %fl:bcode:cdepth
							      %fl:nnn closure
							      #{#<unspecified>}#))
  and %fl:compile-and or %fl:compile-or while %fl:compile-while %fl:cddr for
  %fl:compile-for %fl:caddr %fl:cadddr return ret set! %fl:error "set!: second argument must be a symbol"
  %fl:compile-sym #(seta setc setg) define %fl:expand-define trycatch
  %fl:1arg-lambda? "trycatch: second form must be a 1-argument lambda"
  %fl:compile-app)))) compile-unknown-call) compile-in)
	%fl:compile-or #70=#fn("<000r4e0|}g2g3^c146;" #(%fl:compile-short-circuit
							brt) compile-or)
	%fl:compile-prog1 #71=#fn(";000r3e0|}^e1g231342e2g231F6H0e3|}^e2g231342e4|c542;c6;" #(%fl:compile-in
  %fl:cadr %fl:cddr %fl:compile-begin %fl:emit pop #{#<unspecified>}#) compile-prog1)
	%fl:compile-short-circuit #72=#fn(":000r6g3?6=0e0|}g2g444;g3N?6>0e0|}g2g3M44;c1qe2|3141;" #(%fl:compile-in
  #fn("<000r1e0~\x7f^i03M342e1~c2322e1~i05|332e1~c3322e4~\x7fi02i03Ni04i05362e5~|42;" #(%fl:compile-in
  %fl:emit dup pop %fl:compile-short-circuit %fl:mark-label)) %fl:make-label) compile-short-circuit)
	%fl:compile-sym #73=#fn(";000r4c0qe1g2}`]3441;" #(#fn(":000r1|D6>0e0~i03`[|43;|MD6R0e0~i03a[|M|N342e1~e2\x7fN31a|MS342;c3qe4i023141;" #(%fl:emit
  %fl:bcode:cdepth %fl:nnn #fn(":000r1c0|3116<02e1c2|31316A0e3i10c4c2|3143;e3i10i13b2[|43;" #(#fn(constant?)
  %fl:printable? #fn(top-level-value) %fl:emit loadv)) resolve-global))
							  %fl:lookup-sym) compile-sym)
	%fl:compile-thunk #74=#fn(";000r1e0c1c2L1_L1|L1~3441;" #(%fl:compile #fn(nconc)
								 lambda) #0#)
	%fl:compile-while #75=#fn("9000r4c0qe1|31e1|3142;" #(#fn(":000r2e0~\x7f^e130342e2~|322e0~\x7f^i02342e3~c4}332e3~c5322e0~\x7f^i03342e3~c6|332e2~}42;" #(%fl:compile-in
  %fl:void %fl:mark-label %fl:emit brf pop jmp)) %fl:make-label) compile-while)
	%fl:const-to-idx-vec #76=#fn("9000r1c0qc1e2|313141;" #(#fn("9000r1e0c1qe2~31322|;" #(%fl:table.foreach
  #fn("8000r2~}|\\;" #()) %fl:bcode:ctable))
							       #fn(vector.alloc)
							       %fl:bcode:nconst) const-to-idx-vec)
	%fl:copy-tree #77=#fn("8000r1|?640|;e0|M31e0|N31K;" #(%fl:copy-tree) copy-tree)
	%fl:count #78=#fn("7000r2c0qc141;" #(#fn("9000r1c0qm02|~\x7f`43;" #(#fn(":000r3}Ö50g2;~|}N|}M31690g2aw540g243;" #() count-)))
					     #{#<unspecified>}#) count)
	%fl:define-head-name #79=#fn("7000r1|F690e0|M41;|;" #(%fl:define-head-name) define-head-name)
	%fl:delete-duplicates #80=#fn("8000r1e0|bD326<0c1qc23041;|?640|;c3|M|N42;" #(%fl:length>
  #fn("8000r1c0qc131~_42;" #(#fn("6000r1c0qm02|;" #(#fn("9000r2|?680e0}41;c1i10|M32690~|N}42;c2i10|M]332~|N|M}K42;" #(%fl:reverse!
  #fn(has?) #fn(put!))))) #{#<unspecified>}#))
  #fn(table) #fn("8000r2e0|}32680e1}41;|e1}31K;" #(%fl:member
						   %fl:delete-duplicates))) delete-duplicates)
	%fl:disassemble #81=#fn("=000s1}ÖC0e0|`322e1302];540c22c3}Mc4|31c5|3143;" #(%fl:disassemble
  %fl:newline #{#<unspecified>}# #fn("7000r3c0qc141;" #(#fn(":000r1c0qm02`~axc1u2e2c3e4\x7f`32c5332c6qb4c7\x7f3142;" #(#fn("9000r1|J16602|G@6D0e0c1312e2|i10aw42;e3|41;" #(%fl:princ
  "\n" %fl:disassemble %fl:print) print-val)
  #fn("7000r1e0c141;" #(%fl:princ "\t")) %fl:princ "maxstack " %fl:ref-int32-LE
  "\n" #fn(":000r2c0|}X6E02c1qc2c3q^e433315\x19/;" #(#{#<unspecified>}# #fn(";000r1e0~b432690e130540c22`i20axc3u2e4e5~b4x31c6c7|31c8342~awo002c9q|41;" #(%fl:>
  %fl:newline #{#<unspecified>}# #fn("7000r1e0c141;" #(%fl:princ "\t"))
  %fl:princ %fl:hex5 ":  " #fn(string) "\t" #fn("=000r1c0|c1326P0i20i32e2i31i1032[312i10b4wo10;c0|c3326L0i20i32i31i10[[312i10awo10;c0|c4326K0e5c6i31i10[31312i10awo10;c0|c7326O0e5c6e2i31i103231312i10b4wo10;c0|c8326f0e5c6i31i10[31c9322i10awo102e5c6i31i10[31312i10awo10;c0|c:326ù0e5c6e2i31i103231c9322i10b4wo102e5c6e2i31i103231312i10b4wo102~c;ÇX0e5c9312e5c6e2i31i103231c9322i10b4wo10;c<;|c==6Q0e5c6e2i31i103231c9322i10b4wo10;c0|c>326X0e5c?e@i10b,eAi31i1032R331322i10b2wo10;c0|cB326X0e5c?e@i10b,e2i31i1032R331322i10b4wo10;^;" #(#fn(memq)
  (loadv.l loadg.l setg.l) %fl:ref-int32-LE (loadv loadg setg)
  (loada seta call tcall list + - * / vector argc vargc loadi8 apply tapply)
  %fl:princ #fn(number->string) (loada.l seta.l largc lvargc call.l tcall.l) (loadc
  setc) " " (loadc.l setc.l optargs keyargs) keyargs #{#<unspecified>}# brbound
  (jmp brf brt brne brnn brn) "@" %fl:hex5 %fl:ref-int16-LE (jmp.l brf.l brt.l
								   brne.l
								   brnn.l brn.l)))))
						     #fn(table.foldl)
						     #fn("8000r3g217@02}i21~[<16402|;" #())
						     Instructions))
  #fn(length))) #{#<unspecified>}#)) #fn(function:code)
  #fn(function:vals)) disassemble)
	%fl:div #82=#fn("8000r2|}V|`X16C02}`X16402a17502b/17402`w;" #() div)
	%fl:double #fn(double) %fl:emit
	#83=#fn("G000s2g2Öb0}c0<16C02|`[F16:02|`[Mc1<6;0|`[c2O5:0|`}|`[K\\5Â0c3}c4326A0e5|g2M32L1m2540c62c7qc8}c932312c:qc8}c;32312}c<Ç\\0g2c=>6=0c>m12_m25F0g2c?>6=0c@m12_m2530^540c62}cAÇ\\0g2cB>6=0cCm12_m25F0g2cD>6=0cEm12_m2530^540c62cFq|`[F690|`[M530_|`[322|;" #(car
  cdr cadr #fn(memq) (loadv loadg setg) %fl:bcode:indexfor #{#<unspecified>}#
  #fn("8000r1|16=02e0i02Mc1326;0e2|31o01;c3;" #(%fl:> 255 %fl:cadr
						#{#<unspecified>}#))
  #fn(assq) ((loadv loadv.l) (loadg loadg.l) (setg setg.l) (loada loada.l) (seta
  seta.l)) #fn("8000r1|16O02e0i02Mc13217@02e0e2i0231c1326;0e2|31o01;c3;" #(%fl:>
  255 %fl:cadr #{#<unspecified>}#)) ((loadc loadc.l) (setc setc.l)) loada (0)
  loada0 (1) loada1 loadc (0 0) loadc00 (0 1) loadc01 #fn(">000r2\x7fc0<16ù02|c1<16;02e2}31c3<6E0~`i02Mc4e5}31KK\\5u0|c1ÇB0~`i02Mc6}NKK\\5_0|c7ÇB0~`i02Mc8}NKK\\5I0|c3ÇB0~`i02Mc9}NKK\\530^17^02\x7fc6<16702|c3<6@0~`i02Mc4}NKK\\;~`e:\x7fi02K}32\\;" #(brf
  not %fl:cadr null? brn %fl:cddr brt eq? brne brnn %fl:nreconc))) emit)
	%fl:emit-optional-arg-inits #84=#fn("8000r5g2F6=0c0qe1|3141;c2;" #(#fn("<000r1e0~c1i04332e0~c2|332e3~e4i03i0432\x7fK^e5i0231342e0~c6i04332e0~c7322e8~|322e9~\x7fi02Ni03i04aw45;" #(%fl:emit
  brbound brt %fl:compile-in %fl:list-head %fl:cadar seta pop %fl:mark-label
  %fl:emit-optional-arg-inits)) %fl:make-label #{#<unspecified>}#) emit-optional-arg-inits)
	%fl:encode-byte-code #85=#fn("8000r1c0e1|3141;" #(#fn("8000r1c0e1|3141;" #(#fn(";000r1c0qe1c2|31b3c2|31b2VT2wc33241;" #(#fn("=000r1c0qc1~31`c230c230c330^^47;" #(#fn("?000r7c0g4c1322c2}|X6ˇ02i10}[m52g5c3ÇO0c4g2i10}aw[c5g431332}b2wm15œ0c0g4e6c7e8~6<0c9qg531540g53231322}awm12}|X6:0i10}[530^m62c:g5c;326^0c4g3c5g431g6332c0g4~670e<540e=`31322}awm15_0g5c>ÇG0c0g4e<g631322}awm15C0g6D6<0c?qg531530^5_/2e@cAqg3322cBg441;" #(#fn(io.write)
  #int32(0) #{#<unspecified>}# label #fn(put!)
  #fn(sizeof) %fl:byte #fn(get) Instructions #fn("7000r1|c0Ç50c1;|c2Ç50c3;|c4Ç50c5;|c6Ç50c7;|c8Ç50c9;|c:Ç50c;;i05;" #(jmp
  jmp.l brt brt.l brf brf.l brne brne.l brnn brnn.l brn brn.l))
  #fn(memq) (jmp brf brt brne brnn brn) %fl:int32 %fl:int16 brbound #fn(":000r1c0|c1326H0c2i04e3i0631322\x7fawo01;c0|c4326`0c2i04e5i0631322\x7fawo012c2i04e5i20\x7f[31322\x7fawo01;c0|c6326É0c2i04e3i0631322\x7fawo012c2i04e3i20\x7f[31322\x7fawo012i05c7ÇJ0c2i04e3i20\x7f[31322\x7fawo01;c8;c2i04e5i0631322\x7fawo01;" #(#fn(memq)
  (loadv.l loadg.l setg.l loada.l seta.l largc lvargc call.l tcall.l)
  #fn(io.write) %fl:int32 (loadc setc) %fl:uint8 (loadc.l setc.l optargs
							  keyargs) keyargs
  #{#<unspecified>}#)) %fl:table.foreach #fn("<000r2c0i04|322c1i04i10670e2540e3c4i02}32|x3142;" #(#fn(io.seek)
  #fn(io.write) %fl:int32 %fl:int16 #fn(get)))
  #fn(io.tostring!))) #fn(length) #fn(table)
  #fn(buffer))) %fl:>= #fn(length) 65536)) %fl:list->vector)) %fl:reverse!) encode-byte-code)
	%fl:enum #fn(enum) %fl:error
	#86=#fn(":000s0c0c1|K41;" #(#fn(raise) error) error) %fl:eval #87=#fn("8000r1e0e1|313140;" #(%fl:compile-thunk
  %fl:expand) eval)
	%fl:even? #88=#fn("8000r1c0|a32`W;" #(#fn(logand)) even?) %fl:every
	#89=#fn("8000r2}?17D02|}M3116:02e0|}N42;" #(%fl:every) every)
	%fl:expand #90=#fn("A000r1c0qc1c1c1c1c1c1c1c1c1c1c14;;" #(#fn("8000r;c0m02c1qm12c2L1m22c3qm32c4qm42c5qm52c6qm62c7qm72c8qm82c9m92c:qm:2g:~_42;" #(#fn("8000r2e0|31E17902c1|}32@;" #(resolve-global
  #fn(assq)) top?) #fn("9000r1|?640|;|c0>640|;|MF16;02e1|31c2<6D0c3\x7fe4|3131\x7f|N3142;|M\x7f|N31K;" #(((begin))
  %fl:caar begin #fn(append) %fl:cdar) splice-begin) *expanded* #fn("9000r2|?640|;c0q~c1}32690\x7f|31530|41;" #(#fn("9000r1c0qi10c1\x7f3241;" #(#fn("8000r1c0q|6:0e1~31530_41;" #(#fn(":000r1c0qc1c2c3|32i213241;" #(#fn("8000r1i107=0c0c1qi2042;c2qc3qc431i203141;" #(#fn(map)
  #fn("8000r1i5:|~42;" #()) #fn("7000r1c0q|41;" #(#fn("9000r1c0|F6]02i62e1|31<7A0|i6:|Mi1032O590|e2|31O2|Nm05\x02/2~;" #(#{#<unspecified>}#
  %fl:caar %fl:cdar)))) #fn("6000r1c0qm02|;" #(#fn("9000r1|?640|;|MF16;02c0e1|31<6;0|M~|N31K;c2qi6:|Mi103241;" #(define
  %fl:caar #fn(":000r1c0c1c2e3|3132i2032o202i72|Ki10~N31K;" #(#fn(nconc)
							      #fn(map) #.list
							      %fl:get-defined-vars))))))
  #{#<unspecified>}#)) #fn(nconc) #fn(map) #.list))
  %fl:get-defined-vars)) define)) begin) expand-body)
  #fn(":000r2|?640|;|MF16702|MNF6G0e0|31i0:e1|31}32L2540|Mi04|N}32K;" #(%fl:caar
  %fl:cadar) expand-lambda-list) #fn("8000r1|?660|L1;|MF6@0e0|31i05|N31K;|Mi05|N31K;" #(%fl:caar) l-vars)
  #fn("<000r2c0qe1|31e2|31e3|31i05e1|313144;" #(#fn(":000r4c0qc1c2c3g332\x7f3241;" #(#fn(";000r1c0c1L1i24~|32L1i23i02|32\x7f44;" #(#fn(nconc)
  lambda)) #fn(nconc) #fn(map) #.list)) %fl:cadr %fl:lastcdr %fl:cddr) expand-lambda)
  #fn("<000r2e0|31m02|NA17902e1|31?6Q0e2|31Ö40|;c3e1|31i0:e4|31}32L3;c5qe6|31e7|31e2|31i05e6|313144;" #(%fl:uncurry-define
  %fl:cadr %fl:cddr define %fl:caddr #fn(":000r4c0qc1c2c3g332\x7f3241;" #(#fn(";000r1c0c1L1\x7fi24~|32KL1i23i02|3243;" #(#fn(nconc)
  define)) #fn(nconc) #fn(map) #.list)) %fl:cdadr %fl:caadr) expand-define)
  #fn("8000r2c0qe1|3141;" #(#fn("<000r1c0i13e1~31c2c3c4q|32\x7f3232K;" #(begin
  %fl:cddr #fn(nconc) #fn(map) #fn(":000r1|Me0i2:e1|31i11323130i11L3;" #(%fl:compile-thunk
  %fl:cadr)))) %fl:cadr) expand-let-syntax)
  #fn("6000r2|;" #() local-expansion-env)
  #fn("7000r2|?640|;c0q|M41;" #(#fn("9000r1c0qc1|\x7f3241;" #(#fn("7000r1c0qc1q41;" #(#fn(":000r1c0i10c1326;0c2qi1041;~16602~NF6P0i3:e3~31i20NQ2i39e4~31i213242;~17E02i10C@17;02e5i1031E660|40;c6qe7i203141;" #(#fn(memq)
  (quote lambda define) #fn("8000r1|c0=660i30;|c1=6>0i46i30i3142;i47i30i3142;" #(quote
  lambda)) %fl:cadr %fl:caddr resolve-global #fn("9000r1|6C0i4:e0|i3032i3142;i20c1Ç60i30;i20c2Ç>0i46i30i3142;i20c3Ç>0i47i30i3142;i20c4Ç>0i48i30i3142;~40;" #(%fl:expand-macro-call
  quote lambda define let-syntax)) %fl:macrocall?))
  #fn("7000r0c0qc131i2041;" #(#fn("6000r1c0qm02|;" #(#fn("9000r1|?640|;|M?670|M5<0i4:|Mi3132~|N31K;" #())))
			      #{#<unspecified>}#))))
							      #fn(assq)))) expand-in)))
								  #{#<unspecified>}#) expand)
	%fl:expand-define #91=#fn("=000r1c0e1|31e2|31F6:0e2|315O0e1|31C6;0e330L15=0e4c5e6|313242;" #(#fn("<000r2|C6:0c0|}ML3;c0|Mc1c2L1|NL1c3}31|M34L3;" #(set!
  #fn(nconc) lambda #fn(copy-list))) %fl:cadr %fl:cddr %fl:void %fl:error "compile error: invalid syntax "
  %fl:print-to-string) expand-define)
	%fl:expand-macro-call #92=#fn("7000r2e0690c1qc2t;|}Nv2;" #(*defer-macro-errors*
								   #fn("7000r0~\x7fNv2;" #())
								   #fn("8000r1c0c1|L2L2;" #(raise
  quote))) expand-macro-call)
	%fl:filter #93=#fn("7000r2c0qc141;" #(#fn("9000r1c0qm02|~\x7f_L143;" #(#fn("9000r3g2c0}F6T02i10}M316?0g2}M_KPNm2540c02}Nm15\x0b/2N;" #(#{#<unspecified>}#) filter-)))
					      #{#<unspecified>}#) filter)
	%fl:fits-i8 #94=#fn("8000r1|I16F02e0|b∞3216:02e1|bØ42;" #(%fl:>= %fl:<=) fits-i8)
	%fl:float #fn(float) %fl:foldl
	#95=#fn(":000r3g2Ö40};e0||g2M}32g2N43;" #(%fl:foldl) foldl) %fl:foldr
	#96=#fn(";000r3g2Ö40};|g2Me0|}g2N3342;" #(%fl:foldr) foldr)
	%fl:for-each #97=#fn(";000s2c0qc141;" #(#fn(":000r1c0qm02i02ÖK0c1\x7fF6A02~\x7fM312\x7fNo015\x1e/5;0|~\x7fi02K322];" #(#fn(":000r2}MF6I0|c0c1}32Q22~|c0c2}3242;c3;" #(#fn(map)
  #.car #.cdr #{#<unspecified>}#) for-each-n) #{#<unspecified>}#))
						#{#<unspecified>}#) for-each)
	%fl:function-source #98=#fn("8000r1|J16D02|G@16<02c0c1|3141;" #(#fn("9000r1e0c1|31`3216@02c2|c1|31ax[41;" #(%fl:>
  #fn(length) #fn("7000r1|F16?02|Mc0<16502|N;" #(%source))))
  #fn(function:vals)) function-source)
	%fl:get-defined-vars #99=#fn("8000r1e0~|3141;" #(%fl:delete-duplicates) #1=#(#fn("9000r1|?640_;|Mc0<16602|NF6u0e1|31C16:02e1|31L117^02e1|31F16M02e2e1|3131C16>02e2e1|3131L117402_;|Mc3Ç>0c4c5~|N32v2;_;" #(define
  %fl:cadr %fl:define-head-name begin #fn(nconc)
  #fn(map)) #1#) ()))
	%fl:hex5 #100=#fn("9000r1e0c1|b@32b5c243;" #(%fl:string.lpad #fn(number->string)
						     #\0) hex5)
	%fl:identity #101=#fn("6000r1|;" #() identity) %fl:in-env?
	#102=#fn("8000r2}F16F02c0|}M3217:02e1|}N42;" #(#fn(memq) %fl:in-env?) in-env?)
	%fl:index-of #103=#fn(":000r3}Ö40^;|}MÇ50g2;e0|}Ng2aw43;" #(%fl:index-of) index-of)
	%fl:int16 #fn(int16) %fl:int32
	#fn(int32) %fl:int64 #fn(int64) %fl:int8
	#fn(int8) %fl:io.readall #104=#fn("7000r1c0qc13041;" #(#fn("8000r1c0|~322c1qc2|3141;" #(#fn(io.copy)
  #fn("7000r1|c0>16:02c1i1031670c240;|;" #("" #fn(io.eof?)
					   #fn(eof-object)))
  #fn(io.tostring!))) #fn(buffer)) io.readall)
	%fl:io.readline #105=#fn("8000r1c0|c142;" #(#fn(io.readuntil) #\newline) io.readline)
	%fl:io.readlines #106=#fn("8000r1e0e1|42;" #(%fl:read-all-of
						     %fl:io.readline) io.readlines)
	%fl:iota #107=#fn("8000r1e0e1|42;" #(%fl:map-int %fl:identity) iota)
	%fl:keyword->symbol #108=#fn("9000r1c0|316@0c1c2c3|313141;|;" #(#fn(keyword?)
  #fn(symbol) #fn("<000r1c0|`c1|c2|313243;" #(#fn(string.sub)
					      #fn(string.dec)
					      #fn(length)))
  #fn(string)) keyword->symbol)
	%fl:keyword-arg? #109=#fn("7000r1|F16902c0|M41;" #(#fn(keyword?)) keyword-arg?)
	%fl:lambda-arg-names #110=#fn("9000r1e0c1e2|3142;" #(%fl:map! #fn("7000r1|F690e0|M41;|;" #(%fl:keyword->symbol))
							     %fl:to-proper) lambda-arg-names)
	%fl:lambda-vars #111=#fn("7000r1c0qc141;" #(#fn(":000r1c0qm02|~~^^342e1~41;" #(#fn(";000r4|A17502|C640];|F16602|MC6S0g217502g36<0e0c1}c243;~|N}g2g344;|F16602|MF6á0e3|Mb23216902e4|31C660^5=0e0c5|Mc6}342c7e4|31316<0~|N}g2]44;g36<0e0c1}c843;~|N}]g344;|F6>0e0c9|Mc6}44;|}Ç:0e0c1}42;e0c9|c6}44;" #(%fl:error
  "compile error: invalid argument list "
  ". optional arguments must come after required." %fl:length= %fl:caar "compile error: invalid optional argument "
  " in list " #fn(keyword?) ". keyword arguments must come last."
  "compile error: invalid formal argument ") check-formals)
  %fl:lambda-arg-names)) #{#<unspecified>}#) lambda-vars)
	%fl:last-pair #112=#fn("7000r1|N?640|;e0|N41;" #(%fl:last-pair) last-pair)
	%fl:lastcdr #113=#fn("7000r1|?640|;e0|31N;" #(%fl:last-pair) lastcdr)
	%fl:length= #114=#fn("9000r2}`X640^;}`W650|?;|?660}`W;e0|N}ax42;" #(%fl:length=) length=)
	%fl:length> #115=#fn("9000r2}`X640|;}`W6;0|F16402|;|?660}`X;e0|N}ax42;" #(%fl:length>) length>)
	%fl:list->vector #116=#fn("7000r1c0|v2;" #(#.vector) list->vector)
	%fl:list-head #117=#fn(":000r2e0}`32640_;|Me1|N}ax32K;" #(%fl:<=
								  %fl:list-head) list-head)
	%fl:list-ref #118=#fn("8000r2e0|}32M;" #(%fl:list-tail) list-ref)
	%fl:list-tail #119=#fn("9000r2e0}`32640|;e1|N}ax42;" #(%fl:<=
							       %fl:list-tail) list-tail)
	%fl:list? #120=#fn("7000r1|A17@02|F16902e0|N41;" #(%fl:list?) list?)
	%fl:load #121=#fn("9000r1c0qc1|c23241;" #(#fn("7000r1c0qc1qt;" #(#fn("9000r0c0qc131c1c1c143;" #(#fn("6000r1c0qm02|;" #(#fn(":000r3c0i10317C0~c1i1031|e2}3143;c3i10312e2}41;" #(#fn(io.eof?)
  #fn(read) %fl:load-process #fn(io.close))))) #{#<unspecified>}#))
  #fn("9000r1c0~312c1c2i10|L341;" #(#fn(io.close)
				    #fn(raise) load-error))))
						  #fn(file) :read) load)
	%fl:load-process #122=#fn("7000r1e0|41;" #(%fl:eval) load-process)
	%fl:long #fn(long) %fl:lookup-sym
	#123=#fn("7000r4}Ö50c0;c1q}M41;" #((global)
					   #fn(":000r1c0qe1~|`3341;" #(#fn(";000r1|6@0i13640|;i12|K;e0i10i11Ni1317502~A680i12570i12aw^44;" #(%fl:lookup-sym))
  %fl:index-of))) lookup-sym)
	%fl:macrocall? #124=#fn("8000r1|MC16=02e0e1|M3141;" #(%fl:symbol-syntax
							      resolve-global) macrocall?)
	%fl:macroexpand-1 #125=#fn("8000r1|?640|;c0qe1|3141;" #(#fn("7000r1|680|~Nv2;~;" #())
								%fl:macrocall?) macroexpand-1)
	%fl:make-code-emitter #126=#fn("9000r0_c030`c1Z4;" #(#fn(table) +inf.0) make-code-emitter)
	%fl:make-label #127=#fn("6000r1c040;" #(#fn(gensym)) make-label)
	%fl:make-perfect-hash-table #128=#fn("7000r1c0qc141;" #(#fn("8000r1c0m02c1qc231c3~3141;" #(#fn("9000r2e0e1c2|3131}42;" #(%fl:mod0
  %fl:abs #fn(hash)) $hash-keyword) #fn("6000r1c0qm02|;" #(#fn("9000r1c0qc1b2|T2^3241;" #(#fn("7000r1c0qc131i3041;" #(#fn("6000r1c0qm02|;" #(#fn("8000r1|F6=0c0qe1|3141;i10;" #(#fn(":000r1c0qb2i50|i3032T241;" #(#fn("9000r1i30|[6=0i50i40aw41;i30|~\\2i30|awe0i1031\\2i20i10N41;" #(%fl:cdar))))
  %fl:caar)))) #{#<unspecified>}#)) #fn(vector.alloc))))) #{#<unspecified>}# #fn(length)))
								#{#<unspecified>}#) make-perfect-hash-table)
	%fl:make-system-image #129=#fn(";000r1c0c1|c2c3c434c542;" #(#fn("8000r2c0qe1e242;" #(#fn("7000r2]k02]k12c2qc3q41;" #(*print-pretty*
  *print-readably* #fn("7000r1c0qc1qt|302;" #(#fn(":000r0c0qe1c2qe3c4303132312c5i2041;" #(#fn("=000r1c0c1c2c3|c2c4|3233Q2i20322c5i20e642;" #(#fn(write)
  #fn(nconc) #fn(map) #.list #fn(top-level-value)
  #fn(io.write) *linefeed*)) %fl:filter #fn("9000r1|E16w02c0|31@16l02c1|31G@17C02c2|31c2c1|3131>@16K02c3|i2132@16=02c4c1|3131@;" #(#fn(constant?)
  #fn(top-level-value) #fn(string) #fn(memq)
  #fn(iostream?))) %fl:simple-sort #fn(environment)
  #fn(io.close))) #fn("7000r1~302c0|41;" #(#fn(raise)))))
  #fn("6000r0~k02\x7fk1;" #(*print-pretty* *print-readably*)))) *print-pretty*
  *print-readably*)) #fn(file) :write :create :truncate (*linefeed*
							 *directory-separator*
							 *argv* that
							 *print-pretty*
							 *print-width*
							 *print-readably*
							 *print-level*
							 *print-length*
							 *os-name*)) make-system-image)
	%fl:map! #130=#fn("9000r2}c0}F6B02}|}M31O2}Nm15\x1d/2;" #(#{#<unspecified>}#) map!)
	%fl:map-int #131=#fn("8000r2e0}`32640_;c1q|`31_K_42;" #(%fl:<= #fn(":000r2|m12a\x7faxc0qu2|;" #(#fn("8000r1\x7fi10|31_KP2\x7fNo01;" #())))) map-int)
	%fl:mark-label #132=#fn("9000r2e0|c1}43;" #(%fl:emit label) mark-label)
	%fl:max #133=#fn("<000s1}Ö40|;e0c1|}43;" #(%fl:foldl #fn("7000r2|}X640};|;" #())) max)
	%fl:member #134=#fn("8000r2}?640^;}M|>640};e0|}N42;" #(%fl:member) member)
	%fl:memv #135=#fn("8000r2}?640^;}M|=640};e0|}N42;" #(%fl:memv) memv)
	%fl:min #136=#fn("<000s1}Ö40|;e0c1|}43;" #(%fl:foldl #fn("7000r2|}X640|;};" #())) min)
	%fl:mod #137=#fn("9000r2|e0|}32}T2x;" #(%fl:div) mod) %fl:mod0
	#138=#fn("8000r2||}V}T2x;" #() mod0) %fl:nan? #139=#fn("7000r1|c0>17702|c1>;" #(+nan.0
  -nan.0) nan?)
	%fl:nary-compare #140=#fn("9000r2}A17Q02}NA17I02|}Me0}313216:02e1|}N42;" #(%fl:cadr
  %fl:nary-compare) nary-compare)
	%fl:nary< #141=#fn(":000s0e0c1|42;" #(%fl:nary-compare #fn("7000r2|}X;" #())) nary<)
	%fl:nary= #142=#fn(":000s0e0c1|42;" #(%fl:nary-compare #fn("7000r2|}W;" #())) nary=)
	%fl:negative? #143=#fn("7000r1|`X;" #() negative?) %fl:nestlist
	#144=#fn(";000r3e0g2`32640_;}e1||}31g2ax33K;" #(%fl:<= %fl:nestlist) nestlist)
	%fl:newline #145=#fn("9000â00001000ä0000770e0m02c1|e2322];" #(*output-stream*
  #fn(io.write) *linefeed*) newline)
	%fl:nnn #146=#fn("8000r1e0c1|42;" #(%fl:count #fn("6000r1|A@;" #())) nnn)
	%fl:nreconc #147=#fn("8000r2e0}|42;" #(%fl:reverse!-) nreconc) %fl:odd?
	#148=#fn("7000r1e0|31@;" #(%fl:even?) odd?) %fl:positive? #149=#fn("8000r1e0|`42;" #(%fl:>) positive?)
	%fl:princ #150=#fn("9000s0c0qe141;" #(#fn("7000r1^k02c1qc2q41;" #(*print-readably*
  #fn("7000r1c0qc1qt|302;" #(#fn("8000r0e0c1i2042;" #(%fl:for-each #fn(write)))
			     #fn("7000r1~302c0|41;" #(#fn(raise)))))
  #fn("6000r0~k0;" #(*print-readably*)))) *print-readably*) princ)
	%fl:print #151=#fn(":000s0e0c1|42;" #(%fl:for-each #fn(write)) print)
	%fl:print-exception #152=#fn("=000r1|F16D02|Mc0<16:02e1|b4326S0e2c3e4|31c5e6|31c7352e8e9|31315\x130|F16D02|Mc:<16:02e1|b4326Q0e2e4|31c;e9|31c<342e8e6|31315Ÿ0|F16@02|Mc=<16602|NF6B0e2c>e4|31c?335≤0|F16802|Mc@<6B0e2cA312e2|NQ25ì0|F16802|McB<6J0eCe6|31312e2cDe4|31325l0eE|3116:02e1|b2326L0e8|M312e2cF312cGe4|31315>0e2cH312e8|312e2eI41;" #(type-error
  %fl:length= %fl:princ "type error: " %fl:cadr ": expected " %fl:caddr ", got "
  %fl:print %fl:cadddr bounds-error ": index " " out of bounds for "
  unbound-error "eval: variable " " has no value" error "error: " load-error
  %fl:print-exception "in file " %fl:list? ": " #fn("8000r1c0|3117502|C670e1540e2|41;" #(#fn(string?)
  %fl:princ %fl:print)) "*** Unhandled exception: " *linefeed*) print-exception)
	%fl:print-stack-trace #153=#fn("8000r1c0qc1c142;" #(#fn("=000r2c0qm02c1qm12c2qe3e4~e5670b5540b43231e6e7c8c9c:303232`43;" #(#fn("8000r3c0qc1|31g2K41;" #(#fn("9000r1c0~31c0\x7f31Ç>0c1c2c3|L341;c4qc5~3141;" #(#fn(function:code)
  #fn(raise) thrown-value ffound #fn(":000r1`e0c1|3131c2qu;" #(%fl:1- #fn(length)
							       #fn("9000r1e0~|[316A0i30~|[i21i1043;c1;" #(%fl:closure?
  #{#<unspecified>}#)))) #fn(function:vals)))
  #fn(function:name)) find-in-f) #fn("8000r2c0c1qc2t41;" #(#fn(";000r1|6H0c0e1c2c3e4|3132c53241;c6;" #(#fn(symbol)
  %fl:string.join #fn(map) #fn(string) %fl:reverse! "/" lambda))
							   #fn("8000r0e0c1q\x7f322^;" #(%fl:for-each
  #fn("9000r1i10|~_43;" #()))) #fn("7000r1|F16E02|Mc0<16;02e1|31c2<680e3|41;c4|41;" #(thrown-value
  %fl:cadr ffound %fl:caddr #fn(raise)))) fn-name)
  #fn("8000r3e0c1q|42;" #(%fl:for-each #fn("9000r1e0c1i02c2332e3i11|`[\x7f32e4|31NK312e5302i02awo02;" #(%fl:princ
  "#" " " %fl:print %fl:vector->list %fl:newline)))) %fl:reverse! %fl:list-tail
  *interactive* %fl:filter %fl:closure? #fn(map)
  #fn("7000r1|E16802c0|41;" #(#fn(top-level-value)))
  #fn(environment))) #{#<unspecified>}#) print-stack-trace)
	%fl:print-to-string #154=#fn("7000r1c0qc13041;" #(#fn("8000r1c0~|322c1|41;" #(#fn(write)
  #fn(io.tostring!))) #fn(buffer)) print-to-string)
	%fl:printable? #155=#fn("7000r1c0|3117802c1|31@;" #(#fn(iostream?)
							    #fn(eof-object?)) printable?)
	%fl:proper-part #156=#fn("8000r1|F6<0|Me0|N31K;_;" #(%fl:proper-part) proper-part)
	%fl:quote-value #157=#fn("7000r1e0|31640|;c1|L2;" #(%fl:self-evaluating?
							    quote) quote-value)
	%fl:random #158=#fn("8000r1c0|316<0e1c230|42;c330|T2;" #(#fn(integer?)
								 %fl:mod #fn(rand)
								 #fn(rand.double)) random)
	%fl:read-all #159=#fn("8000r1e0c1|42;" #(%fl:read-all-of #fn(read)) read-all)
	%fl:read-all-of #160=#fn("9000r2c0qc131_|}3142;" #(#fn("6000r1c0qm02|;" #(#fn("9000r2c0i1131680e1|41;~}|Ki10i113142;" #(#fn(io.eof?)
  %fl:reverse!)))) #{#<unspecified>}#) read-all-of)
	%fl:ref-int16-LE #161=#fn(";000r2e0c1|}`w[`32c1|}aw[b832w41;" #(%fl:int16
  #fn(ash)) ref-int16-LE)
	%fl:ref-int32-LE #162=#fn("=000r2e0c1|}`w[`32c1|}aw[b832c1|}b2w[b@32c1|}b3w[bH32R441;" #(%fl:int32
  #fn(ash)) ref-int32-LE)
	%fl:repl #163=#fn("8000r0c0c1c142;" #(#fn("6000r2c0m02c1qm12}302e240;" #(#fn("8000r0e0c1312c2e3312c4c5c6t41;" #(%fl:princ
  "> " #fn(io.flush) *output-stream* #fn("8000r1c0e131@16<02c2e3|3141;" #(#fn(io.eof?)
  *input-stream* #fn("7000r1e0|312|k12];" #(%fl:print that)) %fl:load-process))
  #fn("6000r0c040;" #(#fn(read))) #fn("7000r1c0e1312c2|41;" #(#fn(io.discardbuffer)
							      *input-stream* #fn(raise)))) prompt)
  #fn("7000r0c0qc1t6;0e2302\x7f40;^;" #(#fn("7000r0~3016702e040;" #(%fl:newline))
					#fn("7000r1e0|312];" #(%fl:top-level-exception-handler))
					%fl:newline) reploop) %fl:newline))
					      #{#<unspecified>}#) repl)
	%fl:revappend #164=#fn("8000r2e0}|42;" #(%fl:reverse-) revappend)
	%fl:reverse #165=#fn("8000r1e0_|42;" #(%fl:reverse-) reverse)
	%fl:reverse! #166=#fn("8000r1e0_|42;" #(%fl:reverse!-) reverse!)
	%fl:reverse!- #167=#fn("9000r2c0}F6B02}N}|}m02P2m15\x1d/2|;" #(#{#<unspecified>}#) reverse!-)
	%fl:reverse- #168=#fn("8000r2}Ö40|;e0}M|K}N42;" #(%fl:reverse-) reverse-)
	%fl:self-evaluating? #169=#fn("8000r1|?16602|C@17K02c0|3116A02|C16:02|c1|31<;" #(#fn(constant?)
  #fn(top-level-value)) self-evaluating?)
	%fl:separate #170=#fn("7000r2c0qc141;" #(#fn(":000r1c0m02|~\x7f_L1_L144;" #(#fn(";000r4c0g2g3Kc1}F6Z02|}M316?0g2}M_KPNm25<0g3}M_KPNm32}Nm15\x05/241;" #(#fn("8000r1e0|MN|NN42;" #(%fl:values))
  #{#<unspecified>}#) separate-))) #{#<unspecified>}#) separate)
	%fl:set-syntax! #171=#fn("9000r2c0e1e2|31}43;" #(#fn(put!)
							 *syntax-environment*
							 resolve-global) set-syntax!)
	%fl:simple-sort #172=#fn("7000r1|A17602|NA640|;c0q|M41;" #(#fn("8000r1e0c1qc2q42;" #(%fl:call-with-values
  #fn("8000r0e0c1qi10N42;" #(%fl:separate #fn("7000r1|~X;" #())))
  #fn(":000r2c0e1|31~L1e1}3143;" #(#fn(nconc) %fl:simple-sort))))) simple-sort)
	%fl:splice-form? #173=#fn("8000r1|F16X02|Mc0<17N02|Mc1<17D02|Mc2<16:02e3|b23217702|c2<;" #(unquote-splicing
  unquote-nsplicing unquote %fl:length>) splice-form?)
	%fl:string.join #174=#fn("7000r2|Ö50c0;c1qc23041;" #("" #fn("8000r1c0|~M322e1c2q~N322c3|41;" #(#fn(io.write)
  %fl:for-each #fn("8000r1c0~i11322c0~|42;" #(#fn(io.write)))
  #fn(io.tostring!))) #fn(buffer)) string.join)
	%fl:string.lpad #175=#fn(";000r3c0e1g2}c2|31x32|42;" #(#fn(string)
							       %fl:string.rep
							       #fn(string.count)) string.lpad)
	%fl:string.map #176=#fn("9000r2c0qc130c2}3142;" #(#fn("7000r2c0q`312c1|41;" #(#fn(";000r1c0|\x7fX6S02c1~i10c2i11|3231322c3i11|32m05\x0b/;" #(#{#<unspecified>}#
  #fn(io.putc) #fn(string.char) #fn(string.inc)))
  #fn(io.tostring!))) #fn(buffer) #fn(length)) string.map)
	%fl:string.rep #177=#fn(";000r2}b4X6`0e0}`32650c1;}aW680c2|41;}b2W690c2||42;c2|||43;e3}316@0c2|e4|}ax3242;e4c2||32}b2U242;" #(%fl:<=
  "" #fn(string) %fl:odd? %fl:string.rep) string.rep)
	%fl:string.rpad #178=#fn("<000r3c0|e1g2}c2|31x3242;" #(#fn(string)
							       %fl:string.rep
							       #fn(string.count)) string.rpad)
	%fl:string.tail #179=#fn(";000r2c0|c1|`}3342;" #(#fn(string.sub)
							 #fn(string.inc)) string.tail)
	%fl:string.trim #180=#fn("8000r3c0qc1c142;" #(#fn("8000r2c0qm02c1qm12c2qc3~3141;" #(#fn(";000r4g2g3X16?02c0}c1|g232326A0~|}c2|g232g344;g2;" #(#fn(string.find)
  #fn(string.char) #fn(string.inc)) trim-start)
  #fn("<000r3e0g2`3216D02c1}c2|c3|g23232326?0\x7f|}c3|g23243;g2;" #(%fl:> #fn(string.find)
								    #fn(string.char)
								    #fn(string.dec)) trim-end)
  #fn("<000r1c0i10~i10i11`|34\x7fi10i12|3343;" #(#fn(string.sub)))
  #fn(length))) #{#<unspecified>}#) string.trim)
	%fl:symbol-syntax #181=#fn("9000r1c0e1|^43;" #(#fn(get)
						       *syntax-environment*) symbol-syntax)
	%fl:table.clone #182=#fn("7000r1c0qc13041;" #(#fn("9000r1c0c1q_~332|;" #(#fn(table.foldl)
  #fn("9000r3c0~|}43;" #(#fn(put!))))) #fn(table)) table.clone)
	%fl:table.foreach #183=#fn("9000r2c0c1q_}43;" #(#fn(table.foldl)
							#fn("8000r3~|}322];" #())) table.foreach)
	%fl:table.invert #184=#fn("7000r1c0qc13041;" #(#fn("9000r1c0c1q_~332|;" #(#fn(table.foldl)
  #fn("9000r3c0~}|43;" #(#fn(put!))))) #fn(table)) table.invert)
	%fl:table.keys #185=#fn("9000r1c0c1_|43;" #(#fn(table.foldl)
						    #fn("7000r3|g2K;" #())) table.keys)
	%fl:table.pairs #186=#fn("9000r1c0c1_|43;" #(#fn(table.foldl)
						     #fn("7000r3|}Kg2K;" #())) table.pairs)
	%fl:table.values #187=#fn("9000r1c0c1_|43;" #(#fn(table.foldl)
						      #fn("7000r3}g2K;" #())) table.values)
	%fl:to-proper #188=#fn("8000r1|Ö40|;|?660|L1;|Me0|N31K;" #(%fl:to-proper) to-proper)
	%fl:top-level-exception-handler #189=#fn("7000r1c0qe141;" #(#fn("7000r1e0k12c2qc3q41;" #(*stderr*
  *output-stream* #fn("7000r1c0qc1qt|302;" #(#fn("7000r0e0i20312e1c23041;" #(%fl:print-exception
  %fl:print-stack-trace #fn(stacktrace)))
					     #fn("7000r1~302c0|41;" #(#fn(raise)))))
  #fn("6000r0~k0;" #(*output-stream*)))) *output-stream*) top-level-exception-handler)
	%fl:trace #190=#fn("8000r1c0qc1|31312c2;" #(#fn("7000r1c0qc13041;" #(#fn("@000r1e0~317e0c1i10e2c3|c4c5c6c7i10L2|L3L2c8L1c9c7~L2|L3L4L33142;c:;" #(%fl:traced?
  #fn(set-top-level-value!) %fl:eval lambda begin write cons quote newline
  apply #{#<unspecified>}#)) #fn(gensym)))
						    #fn(top-level-value) ok) trace)
	%fl:traced? #191=#fn("8000r1e0|3116>02c1|31c1~31>;" #(%fl:closure? #fn(function:code)) #(#fn(":000s0c0c1|K312e2302c3|v2;" #(#fn(write)
  x %fl:newline #.apply)) ()))
	%fl:uint16 #fn(uint16) %fl:uint32
	#fn(uint32) %fl:uint64 #fn(uint64) %fl:uint8
	#fn(uint8) %fl:ulong #fn(ulong) %fl:uncurry-define
	#192=#fn("=000r1|NF16D02e0|31F16902e1|31F6P0e2|Me1|31c3c4e5|31e6|3133L341;|;" #(%fl:cadr
  %fl:caadr %fl:uncurry-define #fn(list*) lambda %fl:cdadr %fl:cddr) uncurry-define)
	%fl:untrace #193=#fn("8000r1c0qc1|3141;" #(#fn("9000r1e0|316@0c1~c2|31b2[42;c3;" #(%fl:traced?
  #fn(set-top-level-value!) #fn(function:vals) #{#<unspecified>}#))
						   #fn(top-level-value)) untrace)
	%fl:values #194=#fn("9000s0|F16602|NA650|M;~|K;" #() #2#)
	%fl:vector->list #195=#fn("8000r1c0qc1|31_42;" #(#fn(":000r2a|c0qu2};" #(#fn("8000r1i10~|x[\x7fKo01;" #())))
							 #fn(length)) vector->list)
	%fl:vector.map #196=#fn("8000r2c0qc1}3141;" #(#fn("8000r1c0qc1|3141;" #(#fn(":000r1`~axc0qu2|;" #(#fn(":000r1~|i20i21|[31\\;" #())))
  #fn(vector.alloc))) #fn(length)) vector.map)
	%fl:void #197=#fn("6000r0c0;" #(#{#<unspecified>}#) void) %fl:wchar
	#fn(wchar) %fl:zero? #198=#fn("7000r1|`W;" #() zero?) *banner*
	";  _\n; |_ _ _ |_ _ |  . _ _\n; | (-||||_(_)|__|_)|_)\n;-------------------|----------------------------------------------------------\n\n"
	*builtins* #(0 0 0 0 0 0 0 0 0 0 0 0 #fn("7000r2|}<;" #())
		     #fn("7000r2|}=;" #())
		     #fn("7000r2|}>;" #())
		     #fn("6000r1|?;" #())
		     #fn("6000r1|@;" #())
		     #fn("6000r1|A;" #())
		     #fn("6000r1|B;" #())
		     #fn("6000r1|C;" #())
		     #fn("6000r1|D;" #())
		     #fn("6000r1|E;" #())
		     #fn("6000r1|F;" #())
		     #fn("6000r1|G;" #())
		     #fn("6000r1|H;" #())
		     #fn("6000r1|I;" #())
		     #fn("6000r1|J;" #())
		     #fn("7000r2|}K;" #())
		     #fn("8000s0|;" #()) #fn("6000r1|M;" #())
		     #fn("6000r1|N;" #())
		     #fn("7000r2|}O;" #())
		     #fn("7000r2|}P;" #())
		     #fn("9000s0c0|v2;" #(#.apply))
		     #fn("9000s0c0|v2;" #(#.+))
		     #fn("9000s0c0|v2;" #(#.-))
		     #fn("9000s0c0|v2;" #(#.*))
		     #fn("9000s0c0|v2;" #(#./))
		     #fn("9000s0c0|v2;" #(#.div0))
		     #fn("7000r2|}W;" #())
		     #fn("7000r2|}X;" #())
		     #fn("7000r2|}Y;" #())
		     #fn("9000s0c0|v2;" #(#.vector))
		     #fn("7000r2|}[;" #())
		     #fn("8000r3|}g2\\;" #()))
	*defer-macro-errors* #f *interactive* #f *keep-source* #f
	*print-closures* #t *print-shared* #t
	*syntax-environment* #table(letrec #fn("?000s1c0c0c1L1c2c3|32L1c2c4|32c5}3134L1c2c6|3242;" #(#fn(nconc)
  lambda #fn(map) #.car #fn("9000r1c0c1L1c2|3142;" #(#fn(nconc) set! #fn(copy-list)))
  #fn(copy-list) #fn("6000r1e040;" #(%fl:void))))  quasiquote #fn("8000r1e0|`42;" #(%fl:bq-process))  when #fn("<000s1c0|c1}K^L4;" #(if
  begin))  unwind-protect #fn("8000r2c0qc130c13042;" #(#fn("@000r2c0}c1_\x7fL3L2L1c2c3~c1|L1c4}L1c5|L2L3L3L3}L1L3L3;" #(let
  lambda prog1 trycatch begin raise)) #fn(gensym)))  dotimes #fn("<000s1c0q|Me1|3142;" #(#fn("=000r2c0`c1}aL3c2c3L1|L1L1c4\x7f3133L4;" #(for
  - #fn(nconc) lambda #fn(copy-list))) %fl:cadr))  define-macro #fn("?000s1c0c1|ML2c2c3L1|NL1c4}3133L3;" #(set-syntax!
  quote #fn(nconc) lambda #fn(copy-list)))  receive #fn("@000s2c0c1_}L3c2c1L1|L1c3g23133L3;" #(call-with-values
  lambda #fn(nconc) #fn(copy-list)))  unless #fn("=000s1c0|^c1}KL4;" #(if begin))  let* #fn("A000s1|?6E0c0c1L1_L1c2}3133L1;c0c1L1e3|31L1L1c2|NF6H0c0c4L1|NL1c2}3133L1530}3133e5|31L2;" #(#fn(nconc)
  lambda #fn(copy-list) %fl:caar let* %fl:cadar))  case #fn(":000s1c0qc141;" #(#fn("7000r1c0m02c1qc23041;" #(#fn("9000r2}c0Ç50c0;}Ö40^;}C6=0c1|e2}31L3;}?6=0c3|e2}31L3;}NÖ>0c3|e2}M31L3;e4c5}326=0c6|c7}L2L3;c8|c7}L2L3;" #(else
  eq? %fl:quote-value eqv? %fl:every #.symbol? memq quote memv) vals->cond)
  #fn("<000r1c0|i10L2L1c1c2L1c3c4qi113232L3;" #(let #fn(nconc) cond #fn(map)
						#fn("8000r1i10~|M32|NK;" #())))
  #fn(gensym))) #{#<unspecified>}#))  catch #fn("7000r2c0qc13041;" #(#fn("@000r1c0\x7fc1|L1c2c3c4|L2c5c6|L2c7c8L2L3c5c9|L2~L3L4c:|L2c;|L2L4L3L3;" #(trycatch
  lambda if and pair? eq car quote thrown-value cadr caddr raise))
  #fn(gensym)))  assert #fn("<000r1c0|]c1c2c3|L2L2L2L4;" #(if raise quote
							   assert-failed))  do #fn("A000s2c0qc130}Mc2c3|32c2e4|32c2c5|3245;" #(#fn("B000r5c0|c1g2c2}c3c4L1c5\x7fN3132c3c4L1c5i0231c3|L1g432L133L4L3L2L1c3|L1g332L3;" #(letrec
  lambda if #fn(nconc) begin #fn(copy-list)))
  #fn(gensym) #fn(map) #.car %fl:cadr #fn("7000r1e0|31F680e1|41;|M;" #(%fl:cddr
  %fl:caddr))))  with-input-from #fn("=000s1c0c1L1c2|L2L1L1c3}3143;" #(#fn(nconc)
  with-bindings *input-stream* #fn(copy-list)))  let #fn(":000s1c0q^41;" #(#fn("<000r1~C6D0~m02\x7fMo002\x7fNo01540c02c1qc2c3L1c4c5~32L1c6\x7f3133c4c7~3242;" #(#{#<unspecified>}#
  #fn("8000r2~6@0c0~|L2L1~L3530|}K;" #(letrec))
  #fn(nconc) lambda #fn(map) #fn("6000r1|F650|M;|;" #())
  #fn(copy-list) #fn("7000r1|F680e0|41;e140;" #(%fl:cadr %fl:void))))))  cond #fn("9000s0c0qc141;" #(#fn("7000r1c0qm02|~41;" #(#fn("7000r1|?640^;c0q|M41;" #(#fn(":000r1|Mc0<17702|M]<6@0|NÖ50|M;c1|NK;|NÖ@0c2|Mi10~N31L3;e3|31c4ÇW0e5e6|31316A0c7qe8e6|313141;c9qc:3041;c;|Mc1|NKi10~N31L4;" #(else
  begin or %fl:cadr => %fl:1arg-lambda? %fl:caddr #fn("=000r1c0|~ML2L1c1|c2e3e4~3131Ki20i10N31L4L3;" #(let
  if begin %fl:cddr %fl:caddr)) %fl:caadr #fn("<000r1c0|~ML2L1c1|e2~31|L2i20i10N31L4L3;" #(let
  if %fl:caddr)) #fn(gensym) if))) cond-clauses->if))) #{#<unspecified>}#))  throw #fn(":000r2c0c1c2c3L2|}L4L2;" #(raise
  list quote thrown-value))  time #fn("7000r1c0qc13041;" #(#fn(">000r1c0|c1L1L2L1c2~c3c4c5c1L1|L3c6L4L3L3;" #(let
  time.now prog1 princ "Elapsed time: " - " seconds\n"))
							   #fn(gensym)))  with-output-to #fn("=000s1c0c1L1c2|L2L1L1c3}3143;" #(#fn(nconc)
  with-bindings *output-stream* #fn(copy-list)))  with-bindings #fn(">000s1c0qc1c2|32c1e3|32c1c4|3243;" #(#fn("B000r3c0c1L1c2c3g2|33L1c4c2c5|}3331c6c0c7L1c4\x7f3132c0c7L1c4c2c8|g2333132L3L144;" #(#fn(nconc)
  let #fn(map) #.list #fn(copy-list) #fn("8000r2c0|}L3;" #(set!))
  unwind-protect begin #fn("8000r2c0|}L3;" #(set!))))
  #fn(map) #.car %fl:cadr #fn("6000r1c040;" #(#fn(gensym))))))
	*whitespace* "\t\n\x0b\x0c\x0d ¬Ö¬†·öÄ·†é‚ÄÄ‚ÄÅ‚ÄÇ‚ÄÉ‚ÄÑ‚ÄÖ‚ÄÜ‚Äá‚Äà‚Äâ‚Ää‚Ä®‚Ä©‚ÄØ‚Åü„ÄÄ"
	1+ #3# 1-
	#4# 1arg-lambda? #5# <=
	#6# > #7# >=
	#8# Instructions #table(not 16  vargc 67  load1 49  = 39  setc.l 64  sub2 72  brne.l 83  largc 74  brnn 85  loadc.l 58  loadi8 50  < 40  nop 0  set-cdr! 32  loada 55  bound? 21  / 37  neg 73  brn.l 88  lvargc 75  brt 7  trycatch 68  null? 17  load0 48  jmp.l 8  loadv 51  seta 61  keyargs 91  * 36  function? 26  builtin? 23  aref 43  optargs 89  vector? 24  loadt 45  brf 6  symbol? 19  cdr 30  for 69  loadc00 78  pop 2  pair? 22  cadr 84  closure 65  loadf 46  compare 41  loadv.l 52  setg.l 60  brn 87  eqv? 13  aset! 44  eq? 12  atom? 15  boolean? 18  brt.l 10  tapply 70  dummy_nil 94  loada0 76  brbound 90  list 28  dup 1  apply 33  loadc 57  loadc01 79  dummy_t 92  setg 59  loada1 77  tcall.l 81  jmp 5  fixnum? 25  cons 27  loadg.l 54  tcall 4  call 3  - 35  brf.l 9  + 34  dummy_f 93  add2 71  seta.l 62  loadnil 47  brnn.l 86  setc 63  set-car! 31  vector 42  loadg 53  loada.l 56  argc 66  div0 38  ret 11  number? 20  equal? 14  car 29  call.l 80  brne 82)
	__init_globals #9# __script
	#10# __start #11# abs
	#12# any #13# arg-counts
	#table(#.equal? 2  #.atom? 1  #.set-cdr! 2  #.symbol? 1  #.car 1  #.eq? 2  #.aref 2  #.boolean? 1  #.not 1  #.null? 1  #.eqv? 2  #.number? 1  #.pair? 1  #.builtin? 1  #.aset! 3  #.div0 2  #.= 2  #.bound? 1  #.compare 2  #.vector? 1  #.cdr 1  #.set-car! 2  #.< 2  #.fixnum? 1  #.cons 2)
	argc-error #14# array?
	#15# assoc #16# assv
	#17# bcode:cdepth #18# bcode:code
	#19# bcode:ctable #20# bcode:indexfor
	#21# bcode:nconst #22# bq-bracket
	#23# bq-bracket1 #24# bq-process
	#25# builtin->instruction #26# builtin-argc-ok?
	#27# caaaar #28# caaadr
	#29# caaar #30# caadar
	#31# caaddr #32# caadr
	#33# caar #34# cadaar
	#35# cadadr #36# cadar
	#37# caddar #38# cadddr
	#39# caddr #40# cadr
	#41# call-with-values #42# cdaaar
	#43# cdaadr #44# cdaar
	#45# cdadar #46# cdaddr
	#47# cdadr #48# cdar
	#49# cddaar #50# cddadr
	#51# cddar #52# cdddar
	#53# cddddr #54# cdddr
	#55# cddr #56# char?
	#57# closure? #58# compile
	#59# compile-and #60# compile-app
	#61# compile-arglist #62# compile-begin
	#63# compile-builtin-call #64# compile-f
	#65# compile-f- #66# compile-for
	#67# compile-if #68# compile-in
	#69# compile-or #70# compile-prog1
	#71# compile-short-circuit #72# compile-sym
	#73# compile-thunk #74#
	compile-unknown-call #fn("6000r2^;" #() compile-unknown-call)
	compile-while #75# const-to-idx-vec
	#76# copy-tree #77# count
	#78# define-head-name #79# delete-duplicates
	#80# disassemble #81# div
	#82# emit #83#
	emit-optional-arg-inits #84# encode-byte-code
	#85# error #86# eval
	#87# even? #88# every
	#89# expand #90# expand-define
	#91# expand-macro-call #92# filter
	#93# fits-i8 #94# foldl
	#95# foldr #96# for-each
	#97# function-source #98# get-defined-vars
	#99# hex5 #100# identity
	#101# in-env? #102# index-of
	#103# io.readall #104# io.readline
	#105# io.readlines #106# iota
	#107# keyword->symbol #108# keyword-arg?
	#109# lambda-arg-names #110# lambda-vars
	#111# last-pair #112# lastcdr
	#113# length= #114# length>
	#115# list->vector #116# list-head
	#117# list-ref #118# list-tail
	#119# list? #120# load
	#121# load-process #122# lookup-sym
	#123# macrocall? #124# macroexpand-1
	#125# make-code-emitter #126# make-label
	#127# make-perfect-hash-table #128# make-system-image
	#129# map! #130# map-int
	#131# mark-label #132# max
	#133# member #134# memv
	#135# min #136# mod
	#137# mod0 #138# nan?
	#139# nary-compare #140# nary<
	#141# nary= #142# negative?
	#143# nestlist #144# newline
	#145# nnn #146# nreconc
	#147# odd? #148# positive?
	#149# princ #150# print
	#151# print-exception #152# print-stack-trace
	#153# print-to-string #154# printable?
	#155# proper-part #156# quote-value
	#157# random #158# read-all
	#159# read-all-of #160# ref-int16-LE
	#161# ref-int32-LE #162# repl
	#163# resolve-global #fn("6000r1|;" #()) revappend
	#164# reverse #165# reverse!
	#166# reverse!- #167# reverse-
	#168# self-evaluating? #169# separate
	#170# set-syntax! #171# simple-sort
	#172# special-forms (quote if begin prog1 lambda and or while for
			     return set! define trycatch)
	splice-form? #173# string.join
	#174# string.lpad #175# string.map
	#176# string.rep #177# string.rpad
	#178# string.tail #179# string.trim
	#180# symbol-syntax #181# table.clone
	#182# table.foreach #183# table.invert
	#184# table.keys #185# table.pairs
	#186# table.values #187# to-proper
	#188# top-level-exception-handler
	#189# trace #190# traced?
	#191# uncurry-define #192# untrace
	#193# values #194# vector->list
	#195# vector.map #196# void
	#197# zero? #198#)
