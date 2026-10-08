package example

def get[A](box: Box[A]): A = box.value
def other[A](a: A): A = a
