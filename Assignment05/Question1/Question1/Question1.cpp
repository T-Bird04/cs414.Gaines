//File: Question1.cpp
//Author: Terrence A00103977
// Used Grok for main tree and inorder traversal using prompt: Use the C++ STL std:variant<> template class to define a new C++ class that implements a BST in the same manner as Ocaml. You will need to use the features of the variant type to emulate OCaml pattern matching.


#include <variant>
#include <memory>
#include <iostream> 

template <typename T>
struct Node;

template <typename T>
using Tree = std::variant<std::monostate, std::shared_ptr<const Node<T>>>;

// Forward declarations for functions
template <typename T>
Tree<T> insert(const Tree<T>& t, const T& val);

template <typename T>
bool contains(const Tree<T>& t, const T& val);

// BST class implementation
template <typename T>
class BST {
private:
    Tree<T> root;

    // Private constructor for internal use
    BST(Tree<T> t) : root(std::move(t)) {}

public:
    // Default constructor for empty BST
    BST() : root(std::monostate{}) {}

    // Insert a value, returning a new BST (immutable style)
    BST insert(const T& val) const {
        return BST{ ::insert(root, val) };
    }

    // Check if a value is contained in the BST
    bool contains(const T& val) const {
        return ::contains(root, val);
    }

    // In-order traversal to print the BST
    void inorder() const {
        inorderRec(root);
        std::cout << std::endl;
    }

    void preorder() const
    {
        preorderRec(root);
        std::cout << std::endl;
    }

    void postorder() const
    {
        postorderRec(root);
        std::cout << std::endl;
    }

private:
    // Helper for in-order traversal
    static void inorderRec(const Tree<T>& t) {
        struct Visitor {
            void operator()(std::monostate) const {}
            void operator()(const std::shared_ptr<const Node<T>>& node) const {
                inorderRec(node->left);
                std::cout << node->value << " ";
                inorderRec(node->right);
            }
        };
        std::visit(Visitor{}, t);
    }


    static void preorderRec(const Tree<T>& t)
    {
        struct Visitor {
            void operator()(std::monostate) const {}
            void operator()(const std::shared_ptr<const Node<T>>& node) const {
                std::cout << node->value << " ";
                preorderRec(node->left);
                preorderRec(node->right);
            }
        };
        std::visit(Visitor{}, t);
    }


    static void postorderRec(const Tree<T>& t)
    {
        struct Visitor {
            void operator()(std::monostate) const {}
            void operator()(const std::shared_ptr<const Node<T>>& node) const {
                postorderRec(node->left);
                postorderRec(node->right);
                std::cout << node->value << " ";
            }
        };
        std::visit(Visitor{}, t);
    }







};

// Node structure
template <typename T>
struct Node {
    Tree<T> left;
    T value;
    Tree<T> right;

    Node(Tree<T> l, const T& v, Tree<T> r)
        : left(std::move(l)), value(v), right(std::move(r)) {}
};

// Insert function using std::visit to emulate pattern matching
template <typename T>
Tree<T> insert(const Tree<T>& t, const T& val) {
    struct Visitor {
        const T& val;
        Tree<T> operator()(std::monostate) const {
            return std::make_shared<const Node<T>>(
                Tree<T>{std::monostate{}}, val, Tree<T>{std::monostate{}});
        }
        Tree<T> operator()(const std::shared_ptr<const Node<T>>& node) const {
            if (val < node->value) {
                return std::make_shared<const Node<T>>(
                    insert(node->left, val), node->value, node->right);
            } else if (val > node->value) {
                return std::make_shared<const Node<T>>(
                    node->left, node->value, insert(node->right, val));
            } else {
                return node;
            }
        }
    };
    return std::visit(Visitor{val}, t);
}

// Contains function using std::visit to emulate pattern matching
template <typename T>
bool contains(const Tree<T>& t, const T& val) {
    struct Visitor {
        const T& val;
        bool operator()(std::monostate) const { return false; }
        bool operator()(const std::shared_ptr<const Node<T>>& node) const {
            if (val < node->value) {
                return contains(node->left, val);
            } else if (val > node->value) {
                return contains(node->right, val);
            } else {
                return true;
            }
        }
    };
    return std::visit(Visitor{val}, t);
}



int main() {
    BST<int> bst;
    bst = bst.insert(1);
    bst = bst.insert(2);
    bst = bst.insert(3);
    bst = bst.insert(4);
    bst = bst.insert(5);

    bst.inorder(); 
    bst.preorder();
    bst.postorder();

    return 0;
}
