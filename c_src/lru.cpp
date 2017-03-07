#include <iostream>
#include "lru.h"

Binary get_data(LRUNode<Binary,Binary>* node) {
    if (node) {
	return node->data;
    }
    Binary ret;
    ret.set_data("no such data");
    return ret;
}
	
void node_free(LRUBtree<Binary,Binary> *lru, LRUNode<Binary,Binary> *node) {
    std::cout << "free node " << node->key <<  " -> " << node->data << std::endl;
}

int main(int argc, char** argv) {

  LRUBtree<Binary,Binary> btree(atoi(argv[1]), node_free);
  Binary key1,key1copy,key3,key2,key5,key6,key7,key10,key11;
  Binary data1, data2, data5, data6, data7, data1ov1, data1ov2, data1ov3;

  key1.set_data("key1");
  key1copy.set_data("key1");
  key2.set_data("key2");
  key3.set_data("key3");
  key5.set_data("key5");
  key6.set_data("key6");
  key7.set_data("key7");
  key10.set_data("key10");
  key11.set_data("key11");
 
  
  btree.put(key1, data1.set_data("data1"));
  btree.put(key1, data1ov1.set_data("data1 overwrite1"));
  btree.put(key1, data1ov2.set_data("data1 overwrite2"));
  
  btree.put(key2, data2.set_data("data2"));
  btree.put(key1, data1.set_data("data1"));
  btree.put(key2, data2.set_data("data2"));
  btree.put(key5, data5.set_data("data5"));
  btree.put(key6, data6.set_data("data6"));
  btree.put(key1, data1ov1.set_data("data1 overwrite1"));
  btree.put(key1, data1ov2.set_data("data1 overwrite2"));
  btree.put(key7, data7.set_data("data7"));
  btree.put(key1copy, data1ov3.set_data("data1 overwrite3"));

/*
  std::cout << "bmap key1 => " << get_data(btree.get(key1)) << std::endl;
  std::cout << "bmap key2 => " << get_data(btree.get(key2)) << std::endl;
  std::cout << "bmap key6 => " << get_data(btree.get(key6)) << std::endl;
  std::cout << std::endl;

  std::cout << "get first " << get_data(btree.getFirst()) << std::endl;
  std::cout << "get last " << get_data(btree.getLast()) << std::endl;
*/  
  btree.erase(key6);
  
/*
  std::cout << "get oldest " << get_data(btree.getOldest()) << std::endl;
  std::cout << "get latest " << get_data(btree.getLatest()) << std::endl;
  std::cout << "get latest->next->data " << btree.getLatest()->next->data << std::endl;
*/

  btree.erase(key2);

/*
  std::cout << "get oldest " << btree.getOldest()->data << std::endl;
  std::cout << "get latest " << btree.getLatest()->data << std::endl;
  std::cout << "get latest->next->data " << btree.getLatest()->next->data << std::endl;
*/

  btree.put(key2, data2.set_data("data2"));
  btree.put(key1, data1.set_data("data1"));
  btree.put(key2, data2.set_data("data2"));
  btree.put(key5, data5.set_data("data5"));
  btree.put(key6, data6.set_data("data6"));
  btree.put(key1, data1ov1.set_data("data1 overwrite1"));
  btree.put(key1, data1ov2.set_data("data1 overwrite2"));
  btree.put(key7, data7.set_data("data7"));
  btree.put(key1copy, data1ov3.set_data("data1 overwrite3"));

  std::cout << "get oldest " << btree.getOldest()->data << std::endl;
  std::cout << "get latest " << btree.getLatest()->data << std::endl;
  if (btree.getLatest()->next)
    std::cout << "get latest->next->data " << btree.getLatest()->next->data << std::endl;

  std::cout << "first in bmap " << btree.bmap.begin()->first << " -> " << btree.bmap.begin()->second->data << std::endl;
  std::cout << "last  in bmap " << btree.bmap.rbegin()->first << " -> " << btree.bmap.rbegin()->second->data << std::endl;
  std::cout << std::endl; 

  for (auto it=btree.bmap.begin(); it !=btree.bmap.end(); it++) {
    std::cout << it->first << " => " << it->second->data << std::endl;
  }
 
  std::cout << "key2 < key11 = " << (key2 < key11) << std::endl;

  std::cout << "range of data between key3 and key10" << std::endl;
  LRUBtree<Binary,Binary>::iterator low_it = btree.bmap.lower_bound(key3);
  LRUBtree<Binary,Binary>::iterator up_it  = btree.bmap.upper_bound(key10);
  for (; (low_it != up_it) && low_it !=btree.bmap.end(); low_it++) {
    std::cout << low_it->second->key << " => " << low_it->second->data << std::endl;
  }
  
  std::cout << "range of data between key10 and key3" << std::endl;
  auto it_stop = btree.bmap.lower_bound(key3);
  auto it_start = btree.bmap.upper_bound(key10);
  LRUBtree<Binary,Binary>::reverse_iterator rev_start(it_start);
  LRUBtree<Binary,Binary>::reverse_iterator rev_stop(it_stop);

  for (; (rev_stop != rev_start) && (rev_start != btree.bmap.rend()); rev_start++) {
    std::cout << rev_start->first << " => " << rev_start->second->data << std::endl;
  }
    
  return EXIT_SUCCESS;
}

