class Error(Exception): pass
class AtomicError(Error):
    "Tries to access atomic data with hd or tl"
