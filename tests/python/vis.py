#!/usr/bin/env python2.6

from __future__ import division

import gtk

import numpy as np
import matplotlib
matplotlib.use('GTKAgg')
import matplotlib.cm as cm
import matplotlib.pyplot as plt
from matplotlib.widgets import Button, RadioButtons
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg as FigureCanvas

win = gtk.Window()
win.connect("destroy", gtk.main_quit)
win.set_default_size(600,600)
win.set_title("Resource Visualisation")

fig = plt.figure(figsize=(8,8))
ax = fig.add_axes([0.1, 0.2, 0.8, 0.75], projection='polar')

rax = fig.add_axes([0.7, 0.05, 0.2, 0.05])
rax.grid(False)
rax.set_xticks([])
rax.set_yticks([])

moax = fig.add_axes([0.1, 0.02, 0.25, 0.1])
moax.grid(False)
moax.set_xticks([])
moax.set_yticks([])

logax = fig.add_axes([0.4, 0.02, 0.25, 0.1])
logax.grid(False)
logax.set_xticks([])
logax.set_yticks([])

canvas = FigureCanvas(fig)
win.add(canvas)

class ResVis(object):
    def __init__(self):
        self.dirdata = self.load_data('dirs.dat')
        self.userdata = self.load_data('users.dat')

        self.mode = 'dir'
        self.log = True
        self.draw_dir('root')

    def load_data(self, filename):
        data = {}
        for line in open(filename, 'r').readlines():
            entry = line.split(None, 3)

            if len(entry) > 3:      # current entry has subentries
                entry[3] = entry[3].split()
                entry[3].sort(key=str.lower)
            else:
                entry.append([])

            data[entry[0]] = [entry[0], float(entry[1]), float(entry[2]),
                              entry[3]]
        return data

    def load_dir(self, dirname):
        curdirdata = []
        for d in self.dirdata[dirname][3]:
            curdirdata.append(self.dirdata[d])
        return curdirdata

    def load_user(self, username):
        curdata = []
        for u in self.userdata[username][3]:
            curdata.append(self.userdata[u])
        return curdata

    def draw_dir(self, dirname='root'):
        self.curdir = dirname
        self.reset_ax()
        ax.set_title('Directory size')
        ax.set_xlabel(dirname, weight='bold')

        curdir = self.load_dir(dirname)
        self.draw_data(curdir)

    def draw_user(self, username='root'):
        self.curuser = username
        self.reset_ax()
        ax.set_title('Resource usage')
        ax.set_xlabel(username, weight='bold')

        user = self.load_user(username)
        self.draw_data(user)

    def reset_ax(self):
        ax.cla()

        #ax.axis('off')
        #ax.set_axis_off()
        #ax.set_yscale('log')
        ax.grid(False)
        ax.set_xticks([]) # edge
        ax.set_yticks([]) # radius
        #ax.set_xlabel('Size')
        #ax.set_ylabel('Number of files')

    def draw_data(self, data):
        totalsize = sum(zip(*data)[1])     # get sum of subentry sizes
        unit = 1.5 * np.pi / totalsize

        angle = 0.5 * np.pi
        if self.log:
            maxy = max(map(np.log2, zip(*data)[2]))
        else:
            maxy = max(zip(*data)[2])
        for d in data:
            relangle = unit * d[1]

            if len(d[3]) > 0 or self.mode == 'user':
                # scale colours since the legend occupies a quarter
                scaledangle = (angle - 0.5*np.pi) * (2 / 1.5)
                colour = cm.hsv(scaledangle/(2*np.pi))
            else:
#                colour = cm.Greys(scaledangle/(2*np.pi))
                colour = "#999999"

            if self.log:
                # take logarithm to accomodate for big differences
                y = np.log2(d[2])
            else:
                y = d[2]

            bar = ax.bar(angle, y, width=relangle, bottom=maxy*0.2,
                         color=colour, label=d[0],
                         picker=True)
            angle += relangle

            if self.mode == 'dir':
                desc = '{0}\n{1}G'.format(d[0], d[1])
            elif self.mode == 'user':
                desc = '{0}\n{1}%'.format(d[0], d[1])
            self.draw_desc(bar[0], d, desc)

        self.draw_legend(maxy)

        fig.canvas.draw()

    def draw_desc(self, bar, data, text):
        # show description in center of bar
        bbox = bar.get_bbox()
        x = bbox.xmin + (bbox.xmax - bbox.xmin) / 2
        y = bbox.ymin + (bbox.ymax - bbox.ymin) / 2
        ax.text(x, y, text, horizontalalignment='center',
                verticalalignment='center', weight='bold')

    def draw_legend(self, maxy):
        ax.annotate('', xy=(0, maxy*0.3), xytext=(0.5*np.pi, maxy*0.3),
                    arrowprops=dict(
                        arrowstyle='<->',
                        connectionstyle='angle3,angleA=0,angleB=-90',
                        linewidth=3))
        ax.annotate('', xy=(0.04*np.pi, maxy*0.35), xytext=(0.04*np.pi, maxy),
                    arrowprops=dict(
                        arrowstyle='<->',
                        connectionstyle='arc3',
                        linewidth=3))

        if self.mode == 'dir':
            xtext = 'Size'
            ytext = 'Number of files'
        elif self.mode == 'user':
            xtext = 'Processor usage'
            ytext = 'Memory usage'

        ax.text(0.3*np.pi, maxy*0.35, xtext, weight='normal')
        ax.text(0.06*np.pi, maxy*0.4, ytext, weight='normal', rotation=8)

        if self.mode == 'dir':
            ax.text(0.3*np.pi, maxy*0.6, 'Grey dirs do not\nhave subdirs.')

    def on_pick(self, event):
        clicked = event.artist.get_label()

        if self.mode == 'dir' and len(self.dirdata[clicked][3]) > 0:
                self.draw_dir(clicked)
        elif self.mode == 'user' and len(self.userdata[clicked][3]) > 0:
                self.draw_user(clicked)

    def on_rootclick(self, event):
        if self.mode == 'dir':
            self.draw_dir('root')
        elif self.mode == 'user':
            self.draw_user('root')

    def on_modeclick(self, mode):
        if mode == 'Directory size':
            self.mode = 'dir'
            self.draw_dir('root')
        elif mode == 'Resource usage':
            self.mode = 'user'
            self.draw_user('root')

    def on_logclick(self, mode):
        self.log = mode == 'Logarithmic'
        if self.mode == 'dir':
            self.draw_dir(self.curdir)
        if self.mode == 'user':
            self.draw_user(self.curuser)

vis = ResVis()

root = Button(rax, 'Home')
root.on_clicked(vis.on_rootclick)
mode = RadioButtons(moax, ('Directory size', 'Resource usage'))
mode.on_clicked(vis.on_modeclick)
log = RadioButtons(logax, ('Logarithmic', 'Linear'))
log.on_clicked(vis.on_logclick)

fig.canvas.mpl_connect('pick_event', vis.on_pick)

#plt.show()

win.show_all()
gtk.main()

