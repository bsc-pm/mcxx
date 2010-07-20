#ifndef RED_BLACK_TREE_H
#define RED_BLACK_TREE_H

typedef struct rb_red_blk_tree_tag rb_red_blk_tree;
typedef struct rb_red_blk_node_tag rb_red_blk_node;

rb_red_blk_tree* rb_tree_create( int (*comp_func) (const void*,const void*),
			      void (*key_dtor_func) (const void*),
			      void (*info_dtor_func) (const void*));
rb_red_blk_node *rb_tree_add(rb_red_blk_tree*, const void* key, void* info);
rb_red_blk_node *rb_tree_insert(rb_red_blk_tree*, const void* key, void* info);
void rb_tree_walk(rb_red_blk_tree*, 
        void (*walk_func) (const void* key, void* value, void* data), 
        void *data);
void rb_delete(rb_red_blk_tree* , rb_red_blk_node* );
void rb_tree_destroy(rb_red_blk_tree*);
rb_red_blk_node* rb_tree_query(rb_red_blk_tree*, const void* key);

const void* rb_node_get_key(rb_red_blk_node*);
void* rb_node_get_info(rb_red_blk_node*);

#endif // RED_BLACK_TREE_H
