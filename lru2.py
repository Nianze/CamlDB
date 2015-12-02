from collections import OrderedDict
class LRUCache(object):
	def __init__(self, capacity):
		self.capacity = capacity
		self.cache = OrderedDict()

	def get(self, key):
		if key in self.cache:
			temp = self.cache.pop(key)
			self.cache[key] = temp
			return temp
		return -1

	def set(self, key, value):
		if key in self.cache:
			self.cache.pop(key)
			self.cache[key] = value
			return
		elif len(self.cache) < self.capacity:
			self.cache[key] = value
		else:
			self.cache.popitem(last = False)
			self.cache[key] = value
		return

a = LRUCache(1)
print a
a.set(1, 0)
print a.get(1)
a.set(1, 1)
print a.get(1)
class Node(object):
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.prev = None
        self.next = None

    def __str__(self):
        return "Key: " + str(self.key) + " Value: " + str(self.value) + "\n"

class LRUCache(object):
    def __init__(self, capacity):
        """
        :type capacity: int
        """
        self.capacity = capacity
        # head and tail of a doubly linked list
        self.head = None
        self.tail = None
        # map from key to node
        self.map = {}

    def printCache(self):
        curr = self.head
        while curr:
            print curr
            curr = curr.next

    def remove_node(self, node):
        if node.prev:
            node.prev.next = node.next
        else:
            self.head = node.next
        if node.next:
            node.next.prev = node.prev
        else:
            self.tail = node.prev

    def append_to_tail(self, node):
        if self.head == None:
            self.head = node
            self.tail = node
            return
        node.prev = self.tail
        node.next = None
        self.tail.next = node
        self.tail = node

    def move_to_tail(self, node):
        if node != self.tail:
            self.remove_node(node)
            self.append_to_tail(node)

    def get(self, key):
        """
        :rtype: int
        """
        if key in self.map:
            value = self.map[key].value
            self.move_to_tail(self.map[key])
            return value
        else:
            return -1


    def set(self, key, value):
        """
        :type key: int
        :type value: int
        :rtype: nothing
        """
        if key in self.map:
            curr = self.map[key]
            curr.value = value
            self.move_to_tail(curr)
            return
        elif len(self.map) < self.capacity:
            curr = Node(key, value)
            self.map[key] = curr
            self.append_to_tail(curr)
        else:
            del self.map[self.head.key]
            self.remove_node(self.head)
            curr = Node(key, value)
            self.map[key] = curr
            self.append_to_tail(curr)


# use OrderedDict()
# from collections import OrderedDict
# class LRUCache(object):
#     def __init__(self, capacity):
#         """
#         :type capacity: int
#         """
#         self.capacity = capacity
#         self.cache = OrderedDict()


#     def get(self, key):
#         """
#         :rtype: int
#         """
#         if key in self.cache:
#             value = self.cache.pop(key)
#             self.cache[key] = value
#             return value
#         else:
#             return -1


#     def set(self, key, value):
#         """
#         :type key: int
#         :type value: int
#         :rtype: nothing
#         """
#         if key in self.cache:
#             self.cache.pop(key)
#         elif len(self.cache) == self.capacity:
#             self.cache.popitem(last = False) # pop out the item inserted first
#         self.cache[key] = value
#         return