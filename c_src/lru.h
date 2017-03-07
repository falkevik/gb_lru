#include "btree_map.h"
#include <algorithm>
#include <iostream>
#include "murmurhash2.h"
#include "binary.h"
#include "erl_nif.h"

// extend std::hash to understand Binary type
namespace std { 
    template <> 
    struct hash<Binary> 
    {
	size_t operator()(const Binary& b) const 
	{ 
	    return MurmurHash2(b.bin, b.size, 4242);
    }
    };
}

template <typename K, typename V>
struct LRUNode
{
    K key;
    V data;
    void *kvenv;
    LRUNode<K,V> *prev;
    LRUNode<K,V> *next;
    size_t size;
    LRUNode(void *kvenv = NULL, size_t size=0) : kvenv(kvenv), prev(NULL), next(NULL), size(size) { }
    
/*
    static void *LRUNode<ErlTerm,ErlTerm>::operator new(size_t size) {
	return enif_alloc(size);
    }

    static void operator delete(void *block) {
	enif_free(block);
    }
*/

    void printChain() {
	LRUNode<K,V>* node;
	int i=11;
	std::cout << "(";
	for(node = this; node && i; node = node->next, i--) {
	  std::cout << node->key << " -> ";
	}
	if (node) { 
	    std::cout << " loop detection end ";
	} else {
	    std::cout << " end ";
	}
	std::cout << ")" << std::endl;
    }

    void printNextPrevKey() {
	std::cout << "(";
	printNextKey();
	printPrevKey();
	std::cout << ")";
    }

    void printNextKey() {
	if (next) {
	    std::cout << "next key " << next->key << " ";
	}
    }

    void printPrevKey() {
	if (prev) {
	    std::cout << "prev key " << prev->key << " ";
	}
    }
};

template <class K,class V>
class LRUBtree {
    private:
    LRUNode<K,V> *oldest;
    LRUNode<K,V> *latest;
    unsigned long size;
    unsigned long max_size;
    void (*node_free)(LRUBtree<K,V> *lru, LRUNode<K,V> *node);
    void (*node_kickout)(LRUBtree<K,V> *lru, LRUNode<K,V> *node, void *call_env);
    typedef btree::btree_map<K, LRUNode<K,V>*> LRUBtree_map;
    
    public:
    LRUBtree_map bmap;
    bool pid_set = false;
    ErlNifPid pid;
    typedef typename LRUBtree_map::iterator iterator;
    typedef typename LRUBtree_map::reverse_iterator reverse_iterator;
    
    void printLatest() {
	if (latest) {
	    std::cout << " latest " << latest->key;
	} else {
	    std::cout << " no data in lru ";
	}
    }

    private:
    LRUNode<K,V>* erase(LRUNode<K,V> *node) {
	if (node->next) {
	    node->next->prev = node->prev;
	} 
	if (node->prev) {
	    node->prev->next = node->next;
	}

	if (node == oldest) {
	    oldest = node->prev;
	}
	
	if (node == latest) {
	    latest = node->next;
	}
	
	if (node_free) {
	    node_free(this, node);
	}

	node->next = NULL;
	node->prev = NULL;
	return node;
    }
    
    void printOldest() {
	if(oldest) {
	    std::cout << " oldest " << oldest->key;
	} else {
	    std::cout << " no data in lru ";
	}
    }
 
    void check_size(void *call_env) {
	if (size > max_size) {
	    if (oldest) { // remove check if oldest exist and rely on max_size always being positive
		if (node_kickout)
		    node_kickout(this, oldest, call_env);
		erase(oldest->key);
	    }
	}
    }
    
#define SIZE_100MB 100*1024*1024
    public:
    LRUBtree(unsigned long max_size = SIZE_100MB,
	     void (*node_free)(LRUBtree<K,V> *lru, LRUNode<K,V> *node) = NULL,
	     void (*node_kickout)(LRUBtree<K,V> *lru, LRUNode<K,V> *node, void *call_env) = NULL)
	: oldest(NULL), latest(NULL), size(0), max_size(max_size), node_free(node_free),
	  node_kickout(node_kickout) { }

    ~LRUBtree() {
	LRUNode<K,V> *node;
	LRUNode<K,V> *next;
	node = latest;
	while(node) {
	    if (node_free) {
		node_free(this, node);
	    }
	    next = node->next;
	    delete node;
	    node = next;
	}
    }

    void printSize() {
	std::cout << "size " << size << std::endl;
    }
   
    unsigned long getSize() {
	return size;
    }

    unsigned long getMaxSize() {
	return max_size;
    }

    void setMaxSize(unsigned long max_size) {
	this->max_size = max_size;
    }

    void erase(K key) {
	LRUNode<K,V> *node;
	if ((node = bmap[key])) {
	    erase(node);
	    bmap.erase(key);
	    size -= node->size;
	    delete node;
	}
    }
    
    inline void put(K key, V data, 
	     void *kvenv = NULL, void *call_env = NULL,
	     size_t size = 1) {
	LRUNode<K,V> *node;
	
	this->size += size;	
	check_size(call_env);
	
	// overwrite already existing key
	if ((node = bmap[key])) {
	    this->size -= node->size;
	    erase(node);
	    node->kvenv = kvenv;
	    node->next = latest;
	    node->size = size;
	    if (node->next) {
		node->next->prev = node;
	    }
	    if (!oldest) {
		oldest = node;
	    }
	    latest = node;
	    node->key = key;
	    node->data = data;
	}

	else if (!oldest) {
	    node = new LRUNode<K,V>;
	    node->key = key;
	    node->data = data;
	    node->kvenv = kvenv;
	    node->size = size;
	    oldest = node;
	    latest  = node;
	    bmap[node->key] = node;
	}

	else {
	    node = new LRUNode<K,V>;
	    node->key = key;
	    node->data = data;	    
	    node->kvenv = kvenv;
	    node->size = size;
	    latest->prev = node;
	    node->next = latest;
	    latest = node;
	    bmap[node->key] = node;
	}
    }
    
    LRUNode<K,V>* get(K key) {
	return bmap[key];
    }
    
    LRUNode<K,V>* getOldest() {
      return oldest;
    }
    
    LRUNode<K,V>* getLatest() {
      return latest;
    }
    
    LRUNode<K,V>* getNext(LRUNode<K,V> *node) {
	return node->next;
    }
    
    LRUNode<K,V>* getPrev(LRUNode<K,V> *node) {
	return node->prev;
	
    }
};


