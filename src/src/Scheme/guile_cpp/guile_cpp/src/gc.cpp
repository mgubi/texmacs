#include "guile_cpp/vm.hpp"
#include <csetjmp>
#include <chrono>
#include <algorithm>
#include <cstring>

namespace guile {

// the callback which marks an object for the hooks of the collector
struct MarkCtx {
    Heap* heap;
    std::vector<Obj*>* worklist;
};

static void mark_into(void* data, SCM x) {
    MarkCtx* ctx = static_cast<MarkCtx*>(data);
    ctx->heap->mark_scm(x, *ctx->worklist);
}

void Heap::destroy_obj(Obj* cur) {
    switch (cur->tag) {
    case ObjTag::Pair: delete static_cast<PairObj*>(cur); break;
    case ObjTag::Symbol: delete static_cast<SymbolObj*>(cur); break;
    case ObjTag::Keyword: delete static_cast<KeywordObj*>(cur); break;
    case ObjTag::String: delete static_cast<StringObj*>(cur); break;
    case ObjTag::Vector: delete static_cast<VectorObj*>(cur); break;
    case ObjTag::WeakVector: delete static_cast<WeakVectorObj*>(cur); break;
    case ObjTag::Bignum: delete static_cast<BignumObj*>(cur); break;
    case ObjTag::Fraction: delete static_cast<FractionObj*>(cur); break;
    case ObjTag::Real: delete static_cast<RealObj*>(cur); break;
    case ObjTag::Complex: delete static_cast<ComplexObj*>(cur); break;
    case ObjTag::Subr: delete static_cast<SubrObj*>(cur); break;
    case ObjTag::Closure: delete static_cast<ClosureObj*>(cur); break;
    case ObjTag::Macro: delete static_cast<MacroObj*>(cur); break;
    case ObjTag::Variable: delete static_cast<VariableObj*>(cur); break;
    case ObjTag::Port: delete static_cast<PortObj*>(cur); break;
    case ObjTag::Fluid: delete static_cast<FluidObj*>(cur); break;
    case ObjTag::DynamicState: delete static_cast<DynamicStateObj*>(cur); break;
    case ObjTag::HashTable: delete static_cast<HashTableObj*>(cur); break;
    case ObjTag::Struct: delete static_cast<StructObj*>(cur); break;
    case ObjTag::Continuation: delete static_cast<ContinuationObj*>(cur); break;
    case ObjTag::ContFrame: delete static_cast<ContFrameObj*>(cur); break;
    case ObjTag::DynWind: delete static_cast<DynWindObj*>(cur); break;
    case ObjTag::Promise: delete static_cast<PromiseObj*>(cur); break;
    case ObjTag::Values: delete static_cast<ValuesObj*>(cur); break;
    case ObjTag::Hook: delete static_cast<HookObj*>(cur); break;
    case ObjTag::Guardian: delete static_cast<GuardianObj*>(cur); break;
    case ObjTag::Arbiter: delete static_cast<ArbiterObj*>(cur); break;
    case ObjTag::RandomState: delete static_cast<RandomStateObj*>(cur); break;
    case ObjTag::Regexp: delete static_cast<RegexpObj*>(cur); break;
    case ObjTag::Dir: delete static_cast<DirObj*>(cur); break;
    case ObjTag::UniformVector: delete static_cast<UniformVectorObj*>(cur); break;
    case ObjTag::BitVector: delete static_cast<BitVectorObj*>(cur); break;
    case ObjTag::Array: delete static_cast<ArrayObj*>(cur); break;
    case ObjTag::CharSet: delete static_cast<CharSetObj*>(cur); break;
    case ObjTag::ProcWithSetter: delete static_cast<ProcWithSetterObj*>(cur); break;
    case ObjTag::EvalClosure: delete static_cast<EvalClosureObj*>(cur); break;
    case ObjTag::Smob: delete static_cast<SmobObj*>(cur); break;
    case ObjTag::Stack: delete static_cast<StackObj*>(cur); break;
    case ObjTag::Frame: delete static_cast<FrameObj*>(cur); break;
    case ObjTag::Environment: delete static_cast<EnvironmentObj*>(cur); break;
    case ObjTag::Observer: delete static_cast<ObserverObj*>(cur); break;
    case ObjTag::Mutex: delete static_cast<MutexObj*>(cur); break;
    case ObjTag::CondVar: delete static_cast<CondVarObj*>(cur); break;
    case ObjTag::Thread: delete static_cast<ThreadObj*>(cur); break;
    case ObjTag::DynamicLib: delete static_cast<DynamicLibObj*>(cur); break;
    case ObjTag::Async: delete static_cast<AsyncObj*>(cur); break;
    case ObjTag::Box: delete static_cast<BoxObj*>(cur); break;
    case ObjTag::Bytecode: delete static_cast<BytecodeObj*>(cur); break;
    }
}

Heap::~Heap() {
    Obj* cur = head_;
    while (cur) {
        Obj* nxt = cur->gc_next;
        destroy_obj(cur);
        cur = nxt;
    }
    head_ = nullptr;
}

void Heap::add_root(SCM* slot) {
    explicit_roots_.push_back(slot);
}

void Heap::remove_root(SCM* slot) {
    auto it = std::find(explicit_roots_.begin(), explicit_roots_.end(), slot);
    if (it != explicit_roots_.end()) {
        *it = explicit_roots_.back();
        explicit_roots_.pop_back();
    }
}

void Heap::add_permanent(SCM val) {
    permanent_roots_.push_back(val);
}

void Heap::mark_scm(SCM x, std::vector<Obj*>& worklist) {
    if (!x.is_heap()) return;
    Obj* obj = x.to_obj();
    if (!obj || obj->gc_mark) return;
    obj->gc_mark = 1;
    worklist.push_back(obj);
}

void Heap::trace_obj(Obj* obj, std::vector<Obj*>& worklist) {
    switch (obj->tag) {
    case ObjTag::Pair: {
        auto* p = static_cast<PairObj*>(obj);
        mark_scm(p->car, worklist);
        mark_scm(p->cdr, worklist);
        break;
    }
    case ObjTag::Symbol: {
        auto* s = static_cast<SymbolObj*>(obj);
        mark_scm(s->props, worklist);
        mark_scm(s->fref, worklist);
        mark_scm(s->pref, worklist);
        mark_scm(s->keyword, worklist);
        break;
    }
    case ObjTag::Keyword: {
        auto* k = static_cast<KeywordObj*>(obj);
        mark_scm(k->symbol, worklist);
        break;
    }
    case ObjTag::String:
    case ObjTag::Bignum:
    case ObjTag::Real:
    case ObjTag::Complex:
    case ObjTag::RandomState:
    case ObjTag::Regexp:
    case ObjTag::Dir:
    case ObjTag::UniformVector:
    case ObjTag::BitVector:
    case ObjTag::CharSet:
    case ObjTag::CondVar:
    case ObjTag::DynamicLib:
        break;
    case ObjTag::Vector: {
        auto* v = static_cast<VectorObj*>(obj);
        for (SCM e : v->elts) mark_scm(e, worklist);
        break;
    }
    case ObjTag::WeakVector: {
        auto* wv = static_cast<WeakVectorObj*>(obj);
        if (wv->kind != WeakKind::Vector) {
            // Weak alist vector: mark bucket spine pairs and entry pairs (ephemeron values handled in process_guardians)
            for (SCM b : wv->elts) {
                SCM cur = b;
                while (is_pair(cur)) {
                    auto* node = as_obj<PairObj>(cur);
                    if (!node->gc_mark) node->gc_mark = 1;
                    SCM entry = node->car;
                    if (is_pair(entry)) {
                        auto* ep = as_obj<PairObj>(entry);
                        if (!ep->gc_mark) ep->gc_mark = 1;
                    }
                    cur = node->cdr;
                }
            }
        }
        break;
    }
    case ObjTag::Fraction: {
        auto* f = static_cast<FractionObj*>(obj);
        mark_scm(f->num, worklist);
        mark_scm(f->den, worklist);
        break;
    }
    case ObjTag::Subr: {
        auto* s = static_cast<SubrObj*>(obj);
        mark_scm(s->generic, worklist);
        mark_scm(s->props, worklist);
        break;
    }
    case ObjTag::Closure: {
        auto* c = static_cast<ClosureObj*>(obj);
        mark_scm(c->formals, worklist);
        mark_scm(c->body, worklist);
        mark_scm(c->env, worklist);
        mark_scm(c->props, worklist);
        mark_scm(c->bytecode, worklist);
        mark_scm(c->upvals, worklist);
        for (uint8_t i = 0; i < c->inline_upval_cnt; ++i) {
            mark_scm(c->inline_upvals[i], worklist);
        }
        break;
    }
    case ObjTag::Box: {
        auto* b = static_cast<BoxObj*>(obj);
        mark_scm(b->val, worklist);
        break;
    }
    case ObjTag::Bytecode: {
        auto* bc = static_cast<BytecodeObj*>(obj);
        mark_scm(bc->eval_closure, worklist);
        mark_scm(bc->cached_env, worklist);
        mark_scm(bc->source_formals, worklist);
        mark_scm(bc->source_body, worklist);
        for (SCM v : bc->consts) mark_scm(v, worklist);
        for (SCM v : bc->globals) mark_scm(v, worklist);
        for (SCM v : bc->global_syms) mark_scm(v, worklist);
        for (SCM v : bc->upval_syms) mark_scm(v, worklist);
        for (SCM v : bc->upval_known_bc) mark_scm(v, worklist);
        break;
    }
    case ObjTag::Macro: {
        auto* m = static_cast<MacroObj*>(obj);
        mark_scm(m->transformer, worklist);
        mark_scm(m->name, worklist);
        break;
    }
    case ObjTag::Variable: {
        auto* v = static_cast<VariableObj*>(obj);
        mark_scm(v->value, worklist);
        break;
    }
    case ObjTag::Port: {
        auto* p = static_cast<PortObj*>(obj);
        mark_scm(p->soft_vec, worklist);
        mark_scm(p->filename_scm, worklist);
        break;
    }
    case ObjTag::Fluid: {
        auto* f = static_cast<FluidObj*>(obj);
        mark_scm(f->default_val, worklist);
        break;
    }
    case ObjTag::DynamicState: {
        auto* ds = static_cast<DynamicStateObj*>(obj);
        for (SCM v : ds->values) mark_scm(v, worklist);
        break;
    }
    case ObjTag::HashTable: {
        auto* ht = static_cast<HashTableObj*>(obj);
        if (ht->weak == WeakKind::None) {
            for (SCM b : ht->buckets) mark_scm(b, worklist);
        } else {
            // Mark bucket spine pairs and non-weak parts only
            for (SCM b : ht->buckets) {
                SCM cur = b;
                while (is_pair(cur)) {
                    auto* node = as_obj<PairObj>(cur);
                    if (!node->gc_mark) node->gc_mark = 1;
                    SCM entry = node->car;
                    if (is_pair(entry)) {
                        auto* ep = as_obj<PairObj>(entry);
                        if (!ep->gc_mark) ep->gc_mark = 1;
                        if (ht->weak == WeakKind::KeyAlist) {
                            // Wait to mark value until we know if key survived?
                            // In Guile 1.8, weak-key hash table only marks value if key is alive (ephemeron-like!)
                            // We handle ephemeron marking in clear_weak_refs loop!
                        } else if (ht->weak == WeakKind::ValueAlist) {
                            // Ephemeron-like: mark key only if value is alive
                        }
                    }
                    cur = node->cdr;
                }
            }
        }
        break;
    }
    case ObjTag::Struct: {
        auto* st = static_cast<StructObj*>(obj);
        mark_scm(st->vtable, worklist);
        mark_scm(st->entity_proc, worklist);
        mark_scm(st->entity_setter, worklist);
        mark_scm(st->gf_cache_tag, worklist);
        mark_scm(st->gf_cache_cls0, worklist);
        mark_scm(st->gf_cache_cls1, worklist);
        mark_scm(st->gf_cache_clo, worklist);
        // Trace pointer ('p') and self ('s') slots according to layout
        if (is_struct(st->vtable)) {
            auto* vt = as_obj<StructObj>(st->vtable);
            if (!vt->slots.empty()) {
                SCM layout = SCM(vt->slots[VTABLE_INDEX_LAYOUT]);
                if (is_symbol(layout)) {
                    const std::string& desc = as_obj<SymbolObj>(layout)->name;
                    size_t nfields = desc.size() / 2;
                    for (size_t i = 0; i < st->slots.size(); ++i) {
                        char ftype = 'p';
                        if (st->has_tail && i == st->tail_start) {
                            continue; // tail length word is raw integer
                        }
                        size_t fi = i;
                        if (st->has_tail && i > st->tail_start) {
                            fi = nfields - 1;
                        }
                        if (fi * 2 < desc.size()) {
                            ftype = desc[fi * 2];
                        }
                        if (ftype == 'p' || ftype == 's') {
                            mark_scm(SCM(st->slots[i]), worklist);
                        }
                    }
                }
            }
        }
        break;
    }
    case ObjTag::Continuation: {
        auto* k = static_cast<ContinuationObj*>(obj);
        mark_scm(k->cont_frame, worklist);
        mark_scm(k->dynwind_chain, worklist);
        const SCM* sp = k->snap_stack_data();
        size_t sn = k->snap_stack_size();
        for (size_t i = 0; i < sn; ++i) mark_scm(sp[i], worklist);
        const VmFrameSnap* fp = k->snap_frames_data();
        size_t fn = k->snap_frames_size();
        for (size_t i = 0; i < fn; ++i) {
            mark_scm(fp[i].closure, worklist);
            mark_scm(fp[i].bytecode, worklist);
        }
        break;
    }
    case ObjTag::ContFrame: {
        auto* cf = static_cast<ContFrameObj*>(obj);
        mark_scm(cf->next, worklist);
        mark_scm(cf->expr, worklist);
        mark_scm(cf->env, worklist);
        mark_scm(cf->aux, worklist);
        mark_scm(cf->aux2, worklist);
        mark_scm(cf->aux3, worklist);
        for (SCM v : cf->vals) mark_scm(v, worklist);
        break;
    }
    case ObjTag::DynWind: {
        auto* dw = static_cast<DynWindObj*>(obj);
        mark_scm(dw->a, worklist);
        mark_scm(dw->b, worklist);
        mark_scm(dw->c, worklist);
        mark_scm(dw->d, worklist);
        mark_scm(dw->parent, worklist);
        break;
    }
    case ObjTag::Promise: {
        auto* pr = static_cast<PromiseObj*>(obj);
        mark_scm(pr->expr_or_val, worklist);
        mark_scm(pr->env, worklist);
        break;
    }
    case ObjTag::Values: {
        auto* vl = static_cast<ValuesObj*>(obj);
        mark_scm(vl->values, worklist);
        break;
    }
    case ObjTag::Hook: {
        auto* hk = static_cast<HookObj*>(obj);
        mark_scm(hk->procs, worklist);
        break;
    }
    case ObjTag::Guardian: {
        auto* g = static_cast<GuardianObj*>(obj);
        // Only zombies are strongly marked; live entries are checked in process_guardians()
        for (int i = 0; i < N(g->zombies); i++) mark_scm(g->zombies[i], worklist);
        break;
    }
    case ObjTag::Arbiter: {
        auto* a = static_cast<ArbiterObj*>(obj);
        mark_scm(a->name, worklist);
        break;
    }
    case ObjTag::Array: {
        auto* ar = static_cast<ArrayObj*>(obj);
        mark_scm(ar->base_vec, worklist);
        break;
    }
    case ObjTag::ProcWithSetter: {
        auto* ps = static_cast<ProcWithSetterObj*>(obj);
        mark_scm(ps->getter, worklist);
        mark_scm(ps->setter, worklist);
        break;
    }
    case ObjTag::EvalClosure: {
        auto* ec = static_cast<EvalClosureObj*>(obj);
        mark_scm(ec->module, worklist);
        break;
    }
    case ObjTag::Smob: {
        auto* sm = static_cast<SmobObj*>(obj);
        auto& info = vm().smob_info(sm->smob_type);
        if (info.mark) {
            MarkCtx ctx = {this, &worklist};
            Marker cb = {mark_into, &ctx};
            info.mark(SCM::from_obj(sm), cb);
        }
        break;
    }
    case ObjTag::Stack: {
        auto* st = static_cast<StackObj*>(obj);
        mark_scm(st->id, worklist);
        for (SCM f : st->frames) mark_scm(f, worklist);
        break;
    }
    case ObjTag::Frame: {
        auto* fr = static_cast<FrameObj*>(obj);
        mark_scm(fr->proc, worklist);
        mark_scm(fr->args, worklist);
        mark_scm(fr->source, worklist);
        mark_scm(fr->next, worklist);
        mark_scm(fr->prev, worklist);
        break;
    }
    case ObjTag::Environment: {
        auto* en = static_cast<EnvironmentObj*>(obj);
        mark_scm(en->table, worklist);
        mark_scm(en->local, worklist);
        mark_scm(en->imported, worklist);
        mark_scm(en->child_obs_1, worklist);
        mark_scm(en->child_obs_2, worklist);
        for (SCM o : en->strong_observers) mark_scm(o, worklist);
        // weak_observers are NOT marked here; dead tokens are pruned in clear_weak_refs()
        break;
    }
    case ObjTag::Observer: {
        auto* obs = static_cast<ObserverObj*>(obj);
        mark_scm(obs->env, worklist);
        mark_scm(obs->proc, worklist);
        break;
    }
    case ObjTag::Mutex: {
        auto* m = static_cast<MutexObj*>(obj);
        mark_scm(m->owner, worklist);
        break;
    }
    case ObjTag::Thread: {
        auto* t = static_cast<ThreadObj*>(obj);
        mark_scm(t->result, worklist);
        break;
    }
    case ObjTag::Async: {
        auto* a = static_cast<AsyncObj*>(obj);
        mark_scm(a->thunk, worklist);
        break;
    }
    }
}

void Heap::drain_worklist(std::vector<Obj*>& worklist) {
    while (!worklist.empty()) {
        Obj* obj = worklist.back();
        worklist.pop_back();
        // Fast inline cdr-chaining for PairObj (~85% of live heap objects): zero worklist push/pop on spine!
        while (obj->tag == ObjTag::Pair) {
            auto* p = static_cast<PairObj*>(obj);
            SCM a = p->car;
            SCM d = p->cdr;
            if (a.is_heap()) {
                Obj* ao = a.to_obj();
                if (ao && !ao->gc_mark) {
                    ao->gc_mark = 1;
                    worklist.push_back(ao);
                }
            }
            if (!d.is_heap()) goto next_item;
            Obj* dobj = d.to_obj();
            if (!dobj || dobj->gc_mark) goto next_item;
            dobj->gc_mark = 1;
            obj = dobj;
        }
        trace_obj(obj, worklist);
    next_item:;
    }
}

static bool is_alive(SCM x) noexcept {
    if (!x.is_heap()) return true;
    return x.to_obj()->gc_mark != 0;
}

void Heap::process_guardians(std::vector<Obj*>& worklist) {
    // Ephemeron fixed-point over ONLY weak_containers_ (typically <10 objects instead of 150k+!)
    if (!weak_containers_.empty()) {
        bool changed = true;
        auto process_weak_buckets = [&](std::vector<SCM>& buckets, WeakKind wk) {
            if (wk == WeakKind::KeyAlist) {
                for (SCM b : buckets) {
                    SCM p = b;
                    while (is_pair(p)) {
                        SCM entry = car(p);
                        if (is_pair(entry) && is_alive(car(entry)) && !is_alive(cdr(entry))) {
                            mark_scm(cdr(entry), worklist);
                            changed = true;
                        }
                        p = cdr(p);
                    }
                }
            } else if (wk == WeakKind::ValueAlist) {
                for (SCM b : buckets) {
                    SCM p = b;
                    while (is_pair(p)) {
                        SCM entry = car(p);
                        if (is_pair(entry) && is_alive(cdr(entry)) && !is_alive(car(entry))) {
                            mark_scm(car(entry), worklist);
                            changed = true;
                        }
                        p = cdr(p);
                    }
                }
            }
        };
        while (changed) {
            changed = false;
            for (Obj* cur : weak_containers_) {
                if (!cur->gc_mark) continue;
                if (cur->tag == ObjTag::HashTable) {
                    auto* ht = static_cast<HashTableObj*>(cur);
                    process_weak_buckets(ht->buckets, ht->weak);
                } else if (cur->tag == ObjTag::WeakVector) {
                    auto* wv = static_cast<WeakVectorObj*>(cur);
                    if (wv->kind != WeakKind::Vector) process_weak_buckets(wv->elts, wv->kind);
                }
            }
            if (changed) drain_worklist(worklist);
        }
    }

    // Now process live guardians (typically 0 objects!)
    if (!guardians_.empty()) {
        std::vector<SCM> new_zombies;
        for (GuardianObj* g : guardians_) {
            if (!g->gc_mark || g->destroyed) continue;
            std::vector<SCM> still_live;
            still_live.reserve(g->live.size());
            for (SCM item : g->live) {
                if (!is_alive(item)) {
                    g->zombies << item;
                    new_zombies.push_back(item);
                } else {
                    still_live.push_back(item);
                }
            }
            g->live.swap(still_live);
        }
        if (!new_zombies.empty()) {
            for (SCM z : new_zombies) {
                mark_scm(z, worklist);
            }
            drain_worklist(worklist);
        }
    }

    // Mark filename SCM strings for live entries in compact native src_pos_map
    hashmap<SCM, VM::SrcPosEntry>& spm = vm().src_pos_map;
    if (N(spm) > 0) {
        iterator<SCM> it = iterate(spm);
        while (it->busy()) {
            SCM key = it->next();
            VM::SrcPosEntry e = spm[key];
            if (key.to_obj()->gc_mark && e.filename.is_heap()) {
                Obj* fo = e.filename.to_obj();
                if (fo && !fo->gc_mark) {
                    fo->gc_mark = 1;
                    worklist.push_back(fo);
                }
            }
        }
        if (!worklist.empty()) drain_worklist(worklist);
    }
}

void Heap::clear_weak_refs() {
    for (Obj* cur : weak_containers_) {
        if (!cur->gc_mark) continue;
        if (cur->tag == ObjTag::WeakVector) {
            auto* wv = static_cast<WeakVectorObj*>(cur);
            if (wv->kind == WeakKind::Vector) {
                for (SCM& e : wv->elts) {
                    if (!is_alive(e)) e = SCM_BOOL_F;
                }
            } else {
                for (SCM& bucket : wv->elts) {
                    SCM prev = SCM_EOL;
                    SCM p = bucket;
                    while (is_pair(p)) {
                        SCM entry = car(p);
                        bool keep = true;
                        if (is_pair(entry)) {
                            bool ka = is_alive(car(entry));
                            bool va = is_alive(cdr(entry));
                            if (wv->kind == WeakKind::KeyAlist && !ka) keep = false;
                            else if (wv->kind == WeakKind::ValueAlist && !va) keep = false;
                            else if (wv->kind == WeakKind::DoublyWeakAlist && (!ka || !va)) keep = false;
                        }
                        SCM nxt = cdr(p);
                        if (!keep) {
                            if (prev.is_null()) bucket = nxt;
                            else set_cdr(prev, nxt);
                        } else {
                            prev = p;
                        }
                        p = nxt;
                    }
                }
            }
        } else if (cur->tag == ObjTag::Environment) {
            auto* en = static_cast<EnvironmentObj*>(cur);
            if (!en->weak_observers.empty()) {
                std::vector<SCM> kept;
                kept.reserve(en->weak_observers.size());
                for (SCM o : en->weak_observers) {
                    if (is_alive(o)) kept.push_back(o);
                }
                en->weak_observers.swap(kept);
            }
        } else if (cur->tag == ObjTag::HashTable) {
            auto* ht = static_cast<HashTableObj*>(cur);
            if (ht->weak == WeakKind::None) continue;
            for (SCM& bucket : ht->buckets) {
                SCM prev = SCM_EOL;
                SCM p = bucket;
                while (is_pair(p)) {
                    SCM entry = car(p);
                    bool keep = true;
                    if (is_pair(entry)) {
                        bool ka = is_alive(car(entry));
                        bool va = is_alive(cdr(entry));
                        if (ht->weak == WeakKind::KeyAlist && !ka) keep = false;
                        else if (ht->weak == WeakKind::ValueAlist && !va) keep = false;
                        else if (ht->weak == WeakKind::DoublyWeakAlist && (!ka || !va)) keep = false;
                    }
                    SCM nxt = cdr(p);
                    if (!keep) {
                        if (prev.is_null()) bucket = nxt;
                        else set_cdr(prev, nxt);
                        if (ht->count > 0) --ht->count;
                    } else {
                        prev = p;
                    }
                    p = nxt;
                }
            }
        }
    }
}

void Heap::sweep() {
    // Prune dead entries from native src_pos_map BEFORE pair_pool_.sweep() clears gc_mark!
    {
        hashmap<SCM, VM::SrcPosEntry>& spm = vm().src_pos_map;
        if (N(spm) > 0) {
            array<SCM> dead;
            iterator<SCM> it = iterate(spm);
            while (it->busy()) {
                SCM key = it->next();
                if (!key.to_obj()->gc_mark) dead << key;
            }
            for (int i = 0; i < N(dead); i++) spm->reset(dead[i]);
        }
    }

    size_t freed = 0;
    freed += pair_pool_.sweep();
    if (N(vm().goops_cmethod_closures) > 0) vm().goops_cmethod_closures->clear();

    freed += cont_pool_.sweep();
    freed += closure_pool_.sweep();
    freed += var_pool_.sweep();
    freed += real_pool_.sweep();
    freed += frac_pool_.sweep();
    freed += bignum_pool_.sweep();
    freed += box_pool_.sweep();
    freed += eval_closure_pool_.sweep();
    freed += dynwind_pool_.sweep();
    freed += continuation_pool_.sweep();
    freed += string_pool_.sweep();
    freed += symbol_pool_.sweep();

    // Prune dead weak containers, guardians & ports while gc_mark bits are still intact
    if (!weak_containers_.empty()) {
        size_t w = 0;
        for (size_t i = 0; i < weak_containers_.size(); ++i) {
            if (weak_containers_[i]->gc_mark) weak_containers_[w++] = weak_containers_[i];
        }
        weak_containers_.resize(w);
    }
    if (!guardians_.empty()) {
        size_t w = 0;
        for (size_t i = 0; i < guardians_.size(); ++i) {
            if (guardians_[i]->gc_mark) guardians_[w++] = guardians_[i];
        }
        guardians_.resize(w);
    }
    if (!ports_.empty()) {
        size_t w = 0;
        for (size_t i = 0; i < ports_.size(); ++i) {
            if (ports_[i]->gc_mark) ports_[w++] = ports_[i];
        }
        ports_.resize(w);
    }

    Obj** pp = &head_;
    while (*pp) {
        Obj* cur = *pp;
        if (cur->gc_mark) {
            cur->gc_mark = 0;
            pp = &cur->gc_next;
        } else {
            *pp = cur->gc_next;
            if (cur->tag == ObjTag::Smob) {
                auto* sm = static_cast<SmobObj*>(cur);
                auto& info = vm().smob_info(sm->smob_type);
                if (info.free_fn) info.free_fn(SCM::from_obj(sm));
            }
            destroy_obj(cur);
            ++freed;
        }
    }
    if (freed <= live_count_) live_count_ -= freed;
    else live_count_ = 0;
}

__attribute__((noinline))
void Heap::collect() {
    if (in_gc_) return;
    in_gc_ = true;
    ++vm().mod_cache_epoch;
    auto t0 = std::chrono::steady_clock::now();

    std::vector<Obj*> worklist;
    worklist.reserve(2048);

    // Build sorted address table for non-slab objects once per GC
    non_slab_ptrs_.clear();
    for (Obj* cur = head_; cur; cur = cur->gc_next) {
        non_slab_ptrs_.push_back(reinterpret_cast<uintptr_t>(cur));
    }
    std::sort(non_slab_ptrs_.begin(), non_slab_ptrs_.end());
    uintptr_t ns_lo = non_slab_ptrs_.empty() ? UINTPTR_MAX : non_slab_ptrs_.front();
    uintptr_t ns_hi = non_slab_ptrs_.empty() ? 0 : non_slab_ptrs_.back();

    auto is_valid_heap_ptr = [&](uintptr_t w) noexcept -> bool {
        if (pair_pool_.contains_live(w)) return true;
        if (cont_pool_.contains_live(w)) return true;
        if (closure_pool_.contains_live(w)) return true;
        if (var_pool_.contains_live(w)) return true;
        if (real_pool_.contains_live(w)) return true;
        if (frac_pool_.contains_live(w)) return true;
        if (bignum_pool_.contains_live(w)) return true;
        if (box_pool_.contains_live(w)) return true;
        if (eval_closure_pool_.contains_live(w)) return true;
        if (dynwind_pool_.contains_live(w)) return true;
        if (continuation_pool_.contains_live(w)) return true;
        if (string_pool_.contains_live(w)) return true;
        if (symbol_pool_.contains_live(w)) return true;
        if (w >= ns_lo && w <= ns_hi) {
            return std::binary_search(non_slab_ptrs_.begin(), non_slab_ptrs_.end(), w);
        }
        return false;
    };

    // 1. Spill callee-saved registers onto the stack and conservatively scan [sp, stack_base]
    jmp_buf regs;
    setjmp(regs);

    auto scan_range = [&](const uintptr_t* lo, const uintptr_t* hi) {
        if (lo > hi) std::swap(lo, hi);
        for (const uintptr_t* p = lo; p < hi; ++p) {
            uintptr_t w = *p;
            if (w != 0 && (w & 7ULL) == 0 && is_valid_heap_ptr(w)) {
                mark_scm(SCM(w), worklist);
            }
        }
    };

    // Scan register buffer itself
    scan_range(reinterpret_cast<const uintptr_t*>(&regs),
               reinterpret_cast<const uintptr_t*>(&regs + 1));

    // Scan stack
    if (stack_base_) {
        volatile uintptr_t sp_marker = 0;
        const uintptr_t* lo = reinterpret_cast<const uintptr_t*>(const_cast<uintptr_t*>(&sp_marker));
        const uintptr_t* hi = reinterpret_cast<const uintptr_t*>(stack_base_);
        // Guard against crazy stack distance (> 64MB)
        uintptr_t diff = (hi > lo) ? (reinterpret_cast<uintptr_t>(hi) - reinterpret_cast<uintptr_t>(lo))
                                   : (reinterpret_cast<uintptr_t>(lo) - reinterpret_cast<uintptr_t>(hi));
        if (diff < (64ULL << 20)) {
            scan_range(lo, hi);
        }
    }

    // 2. Explicit roots, permanent roots, and RAII root spans
    for (SCM* slot : explicit_roots_) {
        if (slot) mark_scm(*slot, worklist);
    }
    for (SCM v : permanent_roots_) {
        mark_scm(v, worklist);
    }
    for (GcRootSpan* sp = root_spans_; sp; sp = sp->prev) {
        for (size_t i = 0; i < sp->count; ++i) {
            mark_scm(sp->data[i], worklist);
        }
    }

    // 3. VM internal roots
    if (vm_marker) {
        MarkCtx ctx = {this, &worklist};
        Marker cb = {mark_into, &ctx};
        vm_marker(callback_data, cb);
    }

    // 4. Mark transitive closure
    drain_worklist(worklist);

    // 5. Ephemerons & guardians
    process_guardians(worklist);

    // 6. Clear weak references
    clear_weak_refs();

    // 7. Sweep unmarked objects
    sweep();

    // 8. Adjust threshold dynamically (2x live count, minimum min_gc_threshold_)
    alloc_since_gc_ = 0;
    gc_threshold_ = std::max<size_t>(min_gc_threshold_, live_count_ * 2);
    ++gc_times_;

    auto t1 = std::chrono::steady_clock::now();
    gc_time_sec_ += std::chrono::duration<double>(t1 - t0).count();
    in_gc_ = false;

    if (after_gc_callback) {
        after_gc_callback(callback_data);
    }
}

} // namespace guile
