import matplotlib
import matplotlib.pyplot as plt
# end here
# id-1234
fig=plt.figure(figsize=(9,6))
plt.plot([1,3,2])
fig.tight_layout()
fname = 'pyfig2.png'
plt.savefig(fname)
# id-1234 end here
return fname # return this to org-mode
