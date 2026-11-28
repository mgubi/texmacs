#include "guile_cpp/vm.hpp"
#include <cstdio>
#include <sstream>

namespace guile {

namespace {

void print_obj(SCM obj, SCM port_scm, PortObj* p, bool display_mode, int depth) {
    if (depth > 4000) {
        port_puts(p, "...");
        return;
    }

    if (obj.is_fixnum()) {
        port_puts(p, std::to_string(obj.to_fixnum()));
        return;
    }
    if (obj == SCM_BOOL_F) { port_puts(p, "#f"); return; }
    if (obj == SCM_BOOL_T) { port_puts(p, "#t"); return; }
    if (obj == SCM_EOL) { port_puts(p, "()"); return; }
    if (obj == SCM_EOF) { port_puts(p, "#<eof>"); return; }
    if (obj == SCM_UNDEFINED) { port_puts(p, "#<undefined>"); return; }
    if (obj == SCM_UNSPECIFIED) { port_puts(p, "#<unspecified>"); return; }
    if (obj.is_char()) {
        uint32_t ch = obj.to_char();
        if (display_mode) {
            port_putc(p, static_cast<char>(ch));
        } else {
            port_puts(p, "#\\");
            const char* nm = char_to_name(ch);
            if (nm) {
                port_puts(p, nm);
            } else if (ch >= 33 && ch < 127) {
                port_putc(p, static_cast<char>(ch));
            } else {
                char buf[16];
                std::snprintf(buf, sizeof(buf), "%03o", ch & 0xFF);
                port_puts(p, buf);
            }
        }
        return;
    }

    if (!obj.is_heap()) {
        port_puts(p, "#<unknown>");
        return;
    }

    Obj* o = obj.to_obj();
    switch (o->tag) {
    case ObjTag::Pair: {
        port_putc(p, '(');
        SCM cur = obj;
        SCM slow = obj;
        bool first = true;
        bool cyclic = false;
        int len = 0;
        while (is_pair(cur)) {
            if (!first) port_putc(p, ' ');
            first = false;
            if (++len > 1000000) { port_puts(p, "..."); cyclic = true; break; }
            print_obj(car(cur), port_scm, p, display_mode, depth + 1);
            cur = cdr(cur);
            if ((len & 1) == 0 && is_pair(slow)) slow = cdr(slow);
            if (len > 1 && cur == slow && is_pair(cur)) {
                port_puts(p, " . #0#");
                cyclic = true;
                break;
            }
        }
        if (!cur.is_null() && !cyclic) {
            port_puts(p, " . ");
            print_obj(cur, port_scm, p, display_mode, depth + 1);
        }
        port_putc(p, ')');
        break;
    }
    case ObjTag::Symbol: {
        auto* sym = static_cast<SymbolObj*>(o);
        if (display_mode) {
            port_puts(p, sym->name);
        } else {
            // Check if symbol needs #{...}# escaping
            bool needs_escape = sym->name.empty();
            for (unsigned char c : sym->name) {
                if (c <= 32 || c == '(' || c == ')' || c == '"' || c == ';' || c == '\\' || c == '\'' || c == '`' || c == ',') {
                    needs_escape = true;
                    break;
                }
            }
            if (!needs_escape && scm_string_to_number(sym->name, 10).is_true()) {
                needs_escape = true;
            }
            if (needs_escape) {
                port_puts(p, "#{");
                for (char c : sym->name) {
                    if (c == '\\' || c == '}') port_putc(p, '\\');
                    port_putc(p, c);
                }
                port_puts(p, "}#");
            } else {
                port_puts(p, sym->name);
            }
        }
        break;
    }
    case ObjTag::Keyword: {
        auto* kw = static_cast<KeywordObj*>(o);
        port_puts(p, "#:");
        port_puts(p, as_obj<SymbolObj>(kw->symbol)->name);
        break;
    }
    case ObjTag::String: {
        auto sv = static_cast<StringObj*>(o)->view();
        if (display_mode) {
            port_puts(p, sv);
        } else {
            port_putc(p, '"');
            for (unsigned char c : sv) {
                if (c == '"' || c == '\\') {
                    port_putc(p, '\\');
                    port_putc(p, static_cast<char>(c));
                } else if (c == '\n' && vm().print_opts.escape_newlines) {
                    port_puts(p, "\\n");
                } else if (c == '\r') {
                    port_puts(p, "\\r");
                } else if (c == '\t') {
                    port_puts(p, "\\t");
                } else if (c == '\0') {
                    port_puts(p, "\\0");
                } else {
                    port_putc(p, static_cast<char>(c));
                }
            }
            port_putc(p, '"');
        }
        break;
    }
    case ObjTag::Vector: {
        auto* v = static_cast<VectorObj*>(o);
        port_puts(p, "#(");
        for (size_t i = 0; i < v->elts.size(); ++i) {
            if (i > 0) port_putc(p, ' ');
            print_obj(v->elts[i], port_scm, p, display_mode, depth + 1);
        }
        port_putc(p, ')');
        break;
    }
    case ObjTag::WeakVector: {
        auto* v = static_cast<WeakVectorObj*>(o);
        port_puts(p, "#w(");
        for (size_t i = 0; i < v->elts.size(); ++i) {
            if (i > 0) port_putc(p, ' ');
            print_obj(v->elts[i], port_scm, p, display_mode, depth + 1);
        }
        port_putc(p, ')');
        break;
    }
    case ObjTag::Bignum:
    case ObjTag::Fraction:
    case ObjTag::Real:
    case ObjTag::Complex:
        port_puts(p, scm_number_to_string(obj, 10));
        break;
    case ObjTag::Subr: {
        auto* s = static_cast<SubrObj*>(o);
        port_puts(p, "#<primitive-procedure ");
        port_puts(p, s->name);
        port_putc(p, '>');
        break;
    }
    case ObjTag::Closure: {
        auto* c = static_cast<ClosureObj*>(o);
        port_puts(p, "#<procedure ");
        SCM nm = vm().procedure_property(obj, vm().sym_name);
        if (nm.is_true()) {
            print_obj(nm, port_scm, p, true, depth + 1);
            port_putc(p, ' ');
        }
        print_obj(c->formals, port_scm, p, false, depth + 1);
        port_putc(p, '>');
        break;
    }
    case ObjTag::Macro: {
        auto* m = static_cast<MacroObj*>(o);
        port_puts(p, "#<");
        if (m->kind == MacroKind::Syntax) port_puts(p, "syntax");
        else if (m->kind == MacroKind::Macro) port_puts(p, "macro");
        else if (m->kind == MacroKind::MemoizingMacro) port_puts(p, "macro!");
        else port_puts(p, "builtin-macro!");
        if (m->name.is_true()) {
            port_putc(p, ' ');
            print_obj(m->name, port_scm, p, true, depth + 1);
        }
        port_putc(p, '>');
        break;
    }
    case ObjTag::Variable: {
        auto* v = static_cast<VariableObj*>(o);
        port_puts(p, "#<variable ");
        char buf[32];
        std::snprintf(buf, sizeof(buf), "%lx", static_cast<unsigned long>(obj.bits));
        port_puts(p, buf);
        port_puts(p, " value: ");
        print_obj(v->value, port_scm, p, false, depth + 1);
        port_putc(p, '>');
        break;
    }
    case ObjTag::Port: {
        auto* pt = static_cast<PortObj*>(o);
        port_puts(p, "#<");
        if (pt->closed) port_puts(p, "closed: ");
        if (pt->is_input && pt->is_output) port_puts(p, "input-output: ");
        else if (pt->is_input) port_puts(p, "input: ");
        else if (pt->is_output) port_puts(p, "output: ");
        if (pt->kind == PortKind::String) port_puts(p, "string");
        else if (pt->kind == PortKind::Soft) port_puts(p, "soft");
        else if (pt->kind == PortKind::Pipe) port_puts(p, "pipe");
        else port_puts(p, pt->filename.empty() ? "file" : pt->filename);
        port_putc(p, '>');
        break;
    }
    case ObjTag::Struct: {
        auto* st = static_cast<StructObj*>(o);
        if (is_goops_instance(obj)) {
            auto& v = vm();
            SCM ps = display_mode ? v.subr_display : v.subr_write;
            if (is_subr(ps) && as_obj<SubrObj>(ps)->generic.is_true()) {
                SCM args[2] = {obj, port_scm};
                goops_apply_generic(as_obj<SubrObj>(ps)->generic, args, 2);
                return;
            }
        }
        SCM vt = st->vtable;
        if (is_struct(vt)) {
            auto* vto = as_obj<StructObj>(vt);
            if (vto->slots.size() > VTABLE_INDEX_PRINTER) {
                SCM printer = SCM(vto->slots[VTABLE_INDEX_PRINTER]);
                if (vm().is_procedure(printer)) {
                    vm().call2(printer, obj, port_scm);
                    return;
                }
            }
            SCM name = struct_vtable_name(vt);
            port_puts(p, "#<");
            if (name.is_true()) print_obj(name, port_scm, p, true, depth + 1);
            else port_puts(p, "struct");
            char buf[64];
            std::snprintf(buf, sizeof(buf), " %lx:%lx>", static_cast<unsigned long>(vt.bits), static_cast<unsigned long>(obj.bits));
            port_puts(p, buf);
            return;
        }
        port_puts(p, "#<struct>");
        break;
    }
    case ObjTag::BitVector: {
        auto* bv = static_cast<BitVectorObj*>(o);
        port_puts(p, "#*");
        for (size_t i = 0; i < bv->length; ++i) port_putc(p, bv->get(i) ? '1' : '0');
        break;
    }
    case ObjTag::UniformVector: {
        auto* uv = static_cast<UniformVectorObj*>(o);
        port_putc(p, '#');
        port_puts(p, uniform_type_tag(uv->utype));
        port_putc(p, '(');
        for (size_t i = 0; i < uv->length; ++i) {
            if (i > 0) port_putc(p, ' ');
            print_obj(uniform_vector_ref(uv, i), port_scm, p, display_mode, depth + 1);
        }
        port_putc(p, ')');
        break;
    }
    case ObjTag::Array: {
        auto* ar = static_cast<ArrayObj*>(o);
        port_putc(p, '#');
        port_puts(p, std::to_string(ar->dims.size()));
        if (is_uniform_vector(ar->base_vec)) {
            port_puts(p, uniform_type_tag(as_obj<UniformVectorObj>(ar->base_vec)->utype));
        } else if (is_bitvector(ar->base_vec)) {
            port_putc(p, 'b');
        } else if (is_string(ar->base_vec)) {
            port_putc(p, 'a');
        }
        SCM var = vm().lookup("array->list");
        if (is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined()) {
            SCM lst = vm().call1(as_obj<VariableObj>(var)->value, obj);
            print_obj(lst, port_scm, p, display_mode, depth + 1);
        } else {
            port_puts(p, "(...)");
        }
        break;
    }
    case ObjTag::Fluid:
        port_puts(p, "#<fluid>");
        break;
    case ObjTag::DynamicState:
        port_puts(p, "#<dynamic-state>");
        break;
    case ObjTag::HashTable: {
        auto* ht = static_cast<HashTableObj*>(o);
        char buf[64];
        std::snprintf(buf, sizeof(buf), "#<hash-table %zu/%zu>", ht->count, ht->buckets.size());
        port_puts(p, buf);
        break;
    }
    case ObjTag::Continuation:
        port_puts(p, "#<continuation>");
        break;
    case ObjTag::Promise:
        port_puts(p, "#<promise>");
        break;
    case ObjTag::Values: {
        auto* vl = static_cast<ValuesObj*>(o);
        port_puts(p, "#<values ");
        print_obj(vl->values, port_scm, p, false, depth + 1);
        port_putc(p, '>');
        break;
    }
    case ObjTag::Hook:
        port_puts(p, "#<hook>");
        break;
    case ObjTag::Guardian:
        port_puts(p, "#<guardian>");
        break;
    case ObjTag::Arbiter:
        port_puts(p, "#<arbiter>");
        break;
    case ObjTag::RandomState:
        port_puts(p, "#<random-state>");
        break;
    case ObjTag::Regexp:
        port_puts(p, "#<regexp>");
        break;
    case ObjTag::Dir:
        port_puts(p, "#<directory>");
        break;
    case ObjTag::CharSet:
        port_puts(p, "#<char-set>");
        break;
    case ObjTag::ProcWithSetter:
        port_puts(p, "#<procedure-with-setter>");
        break;
    case ObjTag::EvalClosure:
        port_puts(p, "#<eval-closure>");
        break;
    case ObjTag::Smob: {
        auto* sm = static_cast<SmobObj*>(o);
        auto& info = vm().smob_info(sm->smob_type);
        if (info.print) { info.print(obj, port_scm); return; }
        port_puts(p, "#<");
        port_puts(p, info.name);
        port_putc(p, '>');
        break;
    }
    case ObjTag::Stack:
        port_puts(p, "#<stack>");
        break;
    case ObjTag::Frame:
        port_puts(p, "#<frame>");
        break;
    case ObjTag::Environment: {
        auto* en = static_cast<EnvironmentObj*>(o);
        const char* nm = en->env_kind == EnvKind::Leaf ? "#<leaf-environment>"
                       : en->env_kind == EnvKind::Eval ? "#<eval-environment>"
                       : en->env_kind == EnvKind::Import ? "#<import-environment>"
                       : "#<export-environment>";
        port_puts(p, nm);
        break;
    }
    case ObjTag::Observer:
        port_puts(p, "#<observer>");
        break;
    default:
        port_puts(p, "#<object>");
        break;
    }
}

} // namespace

void VM::write_to_port(SCM obj, SCM port, bool display_mode) {
    if (!is_port(port)) wrong_type_arg(display_mode ? "display" : "write", 2, port, "output port");
    print_obj(obj, port, as_obj<PortObj>(port), display_mode, 0);
}

std::string VM::write_to_string(SCM obj, bool display_mode) {
    SCM p = make_output_string_port();
    write_to_port(obj, p, display_mode);
    return as_obj<PortObj>(p)->str_buf;
}

void VM::init_print() {
    subr_write = define_proc("write", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : current_output_port();
        write_to_port(a[0], port, false);
        return SCM_UNSPECIFIED;
    });
    as_obj<SubrObj>(subr_write)->generic_capable = true;

    subr_display = define_proc("display", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : current_output_port();
        write_to_port(a[0], port, true);
        return SCM_UNSPECIFIED;
    });
    as_obj<SubrObj>(subr_display)->generic_capable = true;
    define_proc("newline", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : current_output_port();
        if (!is_port(port)) wrong_type_arg("newline", 1, port, "output port");
        port_putc(as_obj<PortObj>(port), '\n');
        return SCM_UNSPECIFIED;
    });
    define_proc("object->string", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM p = make_output_string_port();
        if (nargs >= 2 && !a[1].is_undefined()) {
            call2(a[1], a[0], p);
        } else {
            write_to_port(a[0], p, false);
        }
        return make_string(as_obj<PortObj>(p)->str_buf);
    });
    define_proc("truncated-print", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = current_output_port();
        int width = 79;
        bool disp = false;
        SCM kw_port = make_keyword("port");
        SCM kw_width = make_keyword("width");
        SCM kw_display = make_keyword("display?");
        SCM rest = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(rest) && is_pair(cdr(rest))) {
            SCM k = car(rest), v = car(cdr(rest));
            if (scm_eqv(k, kw_port)) port = v;
            else if (scm_eqv(k, kw_width)) width = static_cast<int>(scm_to_int64(v, "truncated-print", 2));
            else if (scm_eqv(k, kw_display)) disp = v.is_true();
            rest = cdr(cdr(rest));
        }
        std::string s = write_to_string(a[0], disp);
        if (width > 3 && static_cast<int>(s.size()) > width) {
            s = s.substr(0, static_cast<size_t>(width - 3)) + "...";
        }
        port_puts(as_obj<PortObj>(port), s);
        return SCM_UNSPECIFIED;
    });
    define_proc("print-state?", 1, 0, false, [](const SCM*, size_t) -> SCM { return SCM_BOOL_F; });
}

} // namespace guile
