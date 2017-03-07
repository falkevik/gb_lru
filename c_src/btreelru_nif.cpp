#include <string>
#include <iostream>
#include <vector>
#include "erl_nif.h"
#include "erlterm.h"
#include "lru.h"


using namespace std;

namespace { /* anonymous namespace starts */

typedef struct _obj_resource {
  bool allocated;
  void *object;
  ErlNifMutex *emtx;
} object_resource;

ErlNifResourceFlags resource_flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

ErlNifResourceType* lruResource;
ErlNifResourceType* iteratorResource;

/* atoms */
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_key;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_invalid;
ERL_NIF_TERM atom_value;
ERL_NIF_TERM atom_max_size;
ERL_NIF_TERM atom_tab;
ERL_NIF_TERM atom_lru_old;

void lru_dtor(ErlNifEnv* env, void *lru);
void iterator_dtor(ErlNifEnv* env, void *it);

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    lruResource = enif_open_resource_type(env,
		    "btreelru_nif",
		    "lru",
		    lru_dtor,
		    resource_flags,
		    NULL);

    iteratorResource = enif_open_resource_type(env,
		    "btreelru_nif", 
		    "iterator",
		    iterator_dtor,
		    resource_flags,
		    NULL);

    atom_ok	  = enif_make_atom(env, "ok");
    atom_key	  = enif_make_atom(env, "key");
    atom_error	  = enif_make_atom(env, "error");
    atom_invalid  = enif_make_atom(env, "invalid");
    atom_value    = enif_make_atom(env, "value");
    atom_max_size = enif_make_atom(env, "max_size");
    atom_tab	  = enif_make_atom(env, "tab");
    atom_lru_old  = enif_make_atom(env, "lru_old");

    return 0;
}

int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    return 0;
}

int upgrade(ErlNifEnv* env, void** priv_data,  void** old_priv_data,ERL_NIF_TERM load_info){
    return 0;
}

void lru_dtor(ErlNifEnv* env, void* _lru_btree) {
    object_resource *lru_btree = (object_resource*) _lru_btree;
    if (lru_btree->allocated) 
	delete (LRUBtree<ErlTerm,ErlTerm>*) lru_btree->object;
}

void iterator_dtor(ErlNifEnv* env, void* _lru_iterator) {
    object_resource *lru_iterator = (object_resource*) _lru_iterator;
    if (lru_iterator->allocated)
	delete (LRUBtree<ErlTerm,ErlTerm>::iterator*) lru_iterator->object;
}

void node_free(LRUBtree<ErlTerm,ErlTerm> *bt_lru, LRUNode<ErlTerm,ErlTerm> *node) {
    enif_free_env((ErlNifEnv*)node->kvenv);
    return;
}

void node_kickout(LRUBtree<ErlTerm,ErlTerm> *bt_lru, LRUNode<ErlTerm,ErlTerm> *node, void *currenv) {
    ErlNifEnv *env = (ErlNifEnv *) currenv; 

    if (bt_lru->pid_set) {
	enif_send(env, &bt_lru->pid, NULL, enif_make_tuple3(env, atom_lru_old, node->key.t, node->data.t));
    }
    
    return;
}

ERL_NIF_TERM next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUNode<ErlTerm,ErlTerm> *node;
    ErlTerm key;
    ErlTerm value;

    if (argc != 2) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;
    
    key.t = argv[1];
    node = bt_lru->get(key);
    
    if (!node) 
	return enif_make_tuple2(env, atom_error, atom_invalid);
    
    node = node->next;
    if (!node) 
	return enif_make_tuple2(env, atom_error, atom_invalid);

    key.t = enif_make_copy(env, node->key.t);
    value.t = enif_make_copy(env, node->data.t);

    return enif_make_tuple2(env, key.t, value.t);
}

ERL_NIF_TERM prev(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUNode<ErlTerm,ErlTerm> *node;
    ErlTerm key;
    ErlTerm value;

    if (argc != 2) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;
    
    key.t = argv[1];
    node = bt_lru->get(key);
    
    if (!node) 
	return enif_make_tuple2(env, atom_error, atom_invalid);
    
    node = node->prev;
    if (!node) 
	return enif_make_tuple2(env, atom_error, atom_invalid);

    key.t = enif_make_copy(env, node->key.t);
    value.t = enif_make_copy(env, node->data.t);

    return enif_make_tuple2(env, key.t, value.t);
}
    

ERL_NIF_TERM create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned long max_size;
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    ERL_NIF_TERM lru_term;

    /* get max_size */
    if (enif_get_ulong(env, argv[0], &max_size) < 1){
	return enif_make_tuple2(env, atom_error, atom_max_size);
    }
    
    if (!(bt_lru = new LRUBtree<ErlTerm,ErlTerm>(max_size, node_free, node_kickout))) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "alloction"));
    }
    
    lru = (object_resource *) enif_alloc_resource(lruResource, sizeof(object_resource));
    lru->object = bt_lru;
    lru->allocated = true;
   
    lru_term = enif_make_resource(env, lru);
    enif_release_resource(lru);

    return enif_make_tuple2(env, atom_ok, lru_term);

}

ERL_NIF_TERM seek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    object_resource *it;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUBtree<ErlTerm,ErlTerm>::iterator *bt_it_;
    LRUBtree<ErlTerm,ErlTerm>::iterator bt_it;
    ErlTerm key;
    ERL_NIF_TERM it_term;
    ERL_NIF_TERM kv;

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }

    key.t = argv[1];

    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *)lru->object;
    
    bt_it = bt_lru->bmap.lower_bound(key);
    if ( bt_it == bt_lru->bmap.end() ) {
	return enif_make_tuple2(env, atom_error, atom_invalid);
    }
    
    
    bt_it_ = new LRUBtree<ErlTerm,ErlTerm>::iterator;
    *bt_it_ = bt_it;
    it = (object_resource *) enif_alloc_resource(iteratorResource, sizeof(object_resource));
    it->object = bt_it_;
    it->allocated = true;
   
    it_term = enif_make_resource(env, it);
    enif_release_resource(it);
    kv = enif_make_tuple2(env, bt_it->second->key.t, bt_it->second->data.t);
    return enif_make_tuple2(env, kv, it_term);

}

ERL_NIF_TERM iterate_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    object_resource *it;
    LRUBtree<ErlTerm,ErlTerm>::iterator *bt_it_;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    ERL_NIF_TERM kv;

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[1], iteratorResource, (void **) &it)) {
	return enif_make_badarg(env);
    }

    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *)lru->object;
    bt_it_ = (LRUBtree<ErlTerm,ErlTerm>::iterator *) it->object;
    
    if (bt_it_ == NULL)
	return enif_make_tuple2(env, atom_error, atom_invalid);

    (*bt_it_)++;
    
    if ( *bt_it_ == bt_lru->bmap.end() ) {
	it->allocated = false;
	delete bt_it_;
	it->object = NULL;
	return enif_make_tuple2(env, atom_error, atom_invalid);
    }
    
    kv = enif_make_tuple2(env, (*bt_it_)->second->key.t, (*bt_it_)->second->data.t);
    return enif_make_tuple2(env, atom_ok, kv);
}

ERL_NIF_TERM close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    
    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }

    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *)lru->object;
    lru->allocated = false;
    delete bt_lru;
    

    return atom_ok;
}

ERL_NIF_TERM read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUNode<ErlTerm,ErlTerm> *node;
    ErlTerm key;
    ERL_NIF_TERM kv;

    if (argc != 2) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;
    
    key.t = argv[1];
    node = bt_lru->get(key);
    
    if (!node) 
	return enif_make_tuple2(env, atom_error, atom_invalid);

    kv = enif_make_tuple2(env, enif_make_copy(env, node->key.t), enif_make_copy(env, node->data.t));

    return enif_make_tuple2(env, atom_ok, kv);
}

ERL_NIF_TERM remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    ErlTerm key;

    if (argc != 2) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;
    
    key.t = argv[1];
    bt_lru->erase(key);
    
    return atom_ok;
}

ERL_NIF_TERM oldest(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUNode<ErlTerm,ErlTerm> *node;
    ERL_NIF_TERM key;
    ERL_NIF_TERM value;
    
    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    node = bt_lru->getOldest();
    
    if (!node) 
	return enif_make_tuple2(env, atom_error, atom_invalid);

    key = enif_make_copy(env, node->key.t);
    value = enif_make_copy(env, node->data.t);

    return enif_make_tuple2(env, key, value);
}

ERL_NIF_TERM latest(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUNode<ErlTerm,ErlTerm> *node;
    ERL_NIF_TERM key;
    ERL_NIF_TERM value;
    
    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    // last is "last in" in the lru
    node = bt_lru->getLatest();
    
    if (!node) 
	return enif_make_tuple2(env, atom_error, atom_invalid);

    key = enif_make_copy(env, node->key.t);
    value = enif_make_copy(env, node->data.t);

    return enif_make_tuple2(env, key, value);
}

ERL_NIF_TERM last(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUNode<ErlTerm,ErlTerm> *node;
    ERL_NIF_TERM key;
    ERL_NIF_TERM value;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }

    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    node = bt_lru->bmap.rbegin()->second;

    if (!node)
	return enif_make_tuple2(env, atom_error, atom_invalid);

    key = enif_make_copy(env, node->key.t);
    value = enif_make_copy(env, node->data.t);

    return enif_make_tuple2(env, key, value);
}

ERL_NIF_TERM first(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    LRUNode<ErlTerm,ErlTerm> *node;
    ERL_NIF_TERM key;
    ERL_NIF_TERM value;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }

    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    node = bt_lru->bmap.begin()->second;

    if (!node)
	return enif_make_tuple2(env, atom_error, atom_invalid);

    key = enif_make_copy(env, node->key.t);
    value = enif_make_copy(env, node->data.t);

    return enif_make_tuple2(env, key, value);
}

ERL_NIF_TERM write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;
    ErlTerm key;
    ErlTerm value;
    ErlNifEnv	 *kv_env;
    size_t size;

    if (argc != 3) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    
    bt_lru = (LRUBtree<ErlTerm, ErlTerm> *) lru->object;

    kv_env   = enif_alloc_env();
    key.t    = enif_make_copy(kv_env, argv[1]);
    value.t  = enif_make_copy(kv_env, argv[2]);
    
    /* do not use the size of term
    size = enif_size_term(key.t);
    size += enif_size_term(value.t);
    */
    
    /* size based on entries */
    size = 1; 
    
    bt_lru->put(key, value, kv_env, env, size);

    return atom_ok;
}

ERL_NIF_TERM register_pid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;

    if (argc != 2) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;
    
    if (!enif_get_local_pid(env, argv[1], &(bt_lru->pid))) {
	return enif_make_badarg(env);
    }
    bt_lru->pid_set = true;

    return atom_ok;
}

ERL_NIF_TERM unregister_pid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;
    
    bt_lru->pid_set = false;

    return atom_ok;
}

ERL_NIF_TERM get_registered_pid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    if (!bt_lru->pid_set) {
	return enif_make_tuple2(env, atom_error, atom_invalid);
    }
    
    return enif_make_pid(env, &(bt_lru->pid));
}

ERL_NIF_TERM get_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    return enif_make_ulong(env, bt_lru->getSize());
}

ERL_NIF_TERM get_max_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    return enif_make_ulong(env, bt_lru->getMaxSize());
}

ERL_NIF_TERM set_max_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    object_resource *lru;
    unsigned long max_size;
    LRUBtree<ErlTerm,ErlTerm> *bt_lru;

    if (argc != 2) {
	return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], lruResource, (void **) &lru)) {
	return enif_make_badarg(env);
    }
    /* get max_size */
    if (enif_get_ulong(env, argv[1], &max_size) < 1){
	return enif_make_tuple2(env, atom_error, atom_max_size);
    }
    
    bt_lru = (LRUBtree<ErlTerm,ErlTerm> *) lru->object;

    bt_lru->setMaxSize(max_size);
    
    return atom_ok;
}

ErlNifFunc nif_funcs[] = {
    {"create", 1, create},
    {"close", 1, close, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"register_pid", 2, register_pid},
    {"unregister_pid", 1, unregister_pid},
    {"get_registered_pid", 1, get_registered_pid},
    {"get_size", 1, get_size},
    {"get_max_size", 1, get_max_size},
    {"set_max_size", 2, set_max_size},
    {"oldest", 1, oldest},
    {"latest", 1, latest},
    {"last", 1, last},
    {"first", 1, first},
    {"read", 2, read},
    {"next", 2, next},
    {"prev", 2, prev},
    {"seek", 2, seek},
    {"iterate_next", 2, iterate_next},
    {"remove", 2, remove},
    {"write", 3, write}
};
} /* anonymouse namespace ends */


ERL_NIF_INIT(btree_lru, nif_funcs, load, reload, upgrade, NULL)
