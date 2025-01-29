
from lib2to3.pytree import LeafPattern
from venv import create
from matplotlib import font_manager as fm, rcParams
import matplotlib.pyplot as plt
from decimal import *
import numpy as np
import glob
import os
import seaborn as sns
from bitstring import BitArray
import matplotlib.colors as colors
from mpl_toolkits.axes_grid1 import make_axes_locatable


def decimalAccuracyFunction(computed, exact):
    decimalAccuracy = 0
    if(computed.strip() == "NR"):
        if(exact.strip() == "NR"):
            decimalAccuracy = np.PINF
        else:
            decimalAccuracy = 0
    else:
        if(exact.strip() == "NR"):
            decimalAccuracy = 0
        else:
            computed_value = Decimal(computed)
            exact_value = Decimal(exact)
            if(exact_value == 0):
                if(computed_value == 0):
                    decimalAccuracy = np.PINF
                else:
                    decimalAccuracy = 0
            else:
                if(computed_value == exact_value):
                    decimalAccuracy = np.PINF
                elif(computed_value == 0):
                    decimalAccuracy = 0
                else:
                    try:
                        if((computed_value/exact_value) < 0):
                            decimalAccuracy = 0
                        else:
                            decimalError = np.abs(np.log10(computed_value/exact_value))
                            if(decimalError == 0):
                                decimalAccuracy = np.PINF
                            else:
                                decimalAccuracy = np.log10(1/decimalError)
                    except:
                        print("An exception occurred")
                        print("CV:" + str(computed_value))
                        print("EV:" + str(exact_value))
    return decimalAccuracy

def plotSingleOperationCDF(operationType):
    data_types = {
        "FixedFloatingPoint_5_10_RE" : "FixedFloatingPoint(5, 10, RE)",
        "FixedP_16_8_RE" : "FixedPoint(8, 8, RE)",
        "IEEE754_5_10_RE" : "IEEE754(5, 10, RE)",
        "Morris_4_16_RZ" : "Morris(16, 4, RZ)",
        "MorrisHEB_4_16_RZ" : "MorrisHEB(16, 4, RZ)",
        "MP_4_16_RE" : "MorrisBiasHEB(16, 4, RE)",
        "Posit_2_16_RE" : "Posit(16, 2, RE)",
        "PM_16_RE" : "MorrisUnaryHEB(16, RE)"
    }
    markers = {
        "FixedFloatingPoint(5, 10, RE)" : "s",
        "Fixed(8,8)" : "o",
        "IEEE754(5, 10, RE)" : "v",
        "Morris(16, 4, RZ)" : "^",
        "MorrisHEB(16, 4, RZ)" : "x",
        "MorrisBiasHEB(16, 4, RE)" : "s",
        "Posit(16, 2, RE)" : "o",
        "MorrisUnaryHEB(16, RE)" : "v"
    }
    linestyles = {
        "FixedFloatingPoint(5, 10, RE)" : "-",
        "Fixed(8,8)" : "-",
        "IEEE754(5, 10, RE)" : "-",
        "Morris(16, 4, RZ)" : "-",
        "MorrisHEB(16, 4, RZ)" : "-",
        "MorrisBiasHEB(16, 4, RE)" : ":",
        "Posit(16, 2, RE)" : ":",
        "MorrisUnaryHEB(16, RE)" : ":"
    }
    operations = {
        "SQRT" : "square_root",
        "LN" : "natural_logarithm",
        "INV" : "inverse",
        "EXP" : "exponential",
        "SIN" : "sinus",
        "SINH" : "hyperbolic_sinus",
        "CRT" : "cube_root"
    }
    directory = "results\\"
    path = directory + operations.get(operationType, "shit")
    fig = plt.figure()
    ax = plt.subplot(111)
    ax.set_title("CDF of decimal accuracy for " + operationType, fontsize = 10)
    plt.xlabel('Decimal Accuracy', fontsize=8)

    for filename in glob.glob(os.path.join(path, '*.txt')):
        print(filename)
        with open(os.path.join(os.getcwd(), filename), 'r') as f: # open in readonly mode
            #print("a intrat")
            x = []
            y = []
            data_type = os.path.basename(filename).split('.')[0]
            for row in f:
                row = row.split(' ')
                if(len(row) < 6):
                    continue
                # print(row[1].strip() == "NR")
                x.append(row[0])
                computed = row[2]
                #print(row[3])
                exact = row[3]
                computedDividExact = row[4]
                absoluteError = row[5]
                decimalAccuracy = 0
                if(computed.strip() == "NR"):
                    if(exact.strip() == "NR"):
                        decimalAccuracy = np.PINF
                    else:
                        decimalAccuracy = 0
                else:
                    if(exact.strip() == "NR"):
                        decimalAccuracy = 0
                    else:
                        computed_value = Decimal(computed)
                        exact_value = Decimal(exact)
                        if(exact_value == 0):
                            if(computed_value == 0):
                                decimalAccuracy = np.PINF
                            else:
                                decimalAccuracy = 0
                        else:
                            if(computed_value == exact_value):
                                decimalAccuracy = np.PINF
                            elif(computed_value == 0):
                                decimalAccuracy = 0
                            else:
                                decimalError = np.abs(np.log10(computed_value/exact_value))
                                decimalAccuracy = np.log10(1/decimalError)
                #print(row[0] + " DA:" + str(decimalAccuracy))
                y.append(decimalAccuracy)
            y = np.array(y)
            y = y.astype(np.float64)
            #y = y[np.isfinite(y)]
            sns.kdeplot(ax=ax,data = y, cumulative = True, label = data_types[data_type], marker=markers[data_types[data_type]], linestyle=linestyles[data_types[data_type]], markersize=3, markevery=5)
            #ax.hist(y, bins=100, cumulative=True, label = data_type, 
            #        histtype='step', alpha=0.55, color='purple')

    #ax.legend()
    # Shrink current axis's height by 10% on the bottom
    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    ax.set_xticks(range(-4, 10))

    # Put a legend below current axis
    ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.2), fancybox=True, shadow=True, ncol=4, fontsize=8)
    image_format = 'svg' # e.g .png, .svg, etc.
    image_name = operations.get(operationType, "shit") + "CDF.svg"
    print("save image")
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def plotDoubleOperationColorMap(operationType, data_type):
    data_types = {
        #"FixedFloatingPoint_4_7_RE" : "FloatingPoint(4, 7)",
        #"FixedP_12_6_RE" : "FixedP(6,6)",
        #"IEEE754_4_7_RE" : "IEEE754(4, 7)",
        #"Morris_3_12_RZ" : "Morris(3,12)",
        #"MorrisHEB_3_12_RZ" : "MorrisHEB(3,12)",
        #"MP_3_12_RE" : "MorrisBiasHEB(3,12)",
        #"Posit_2_12_RE" : "Posit(12)",
        #"PM_12_RE" : "MorrisUnaryHEB(12)",
        "Posit_1_8_RE" : "Posit(8, 1, RE)"
    }
    directory = "results\\"
    operations = {
        "ADD" : "addition"#,
        #"MUL" : "multiplication",
        #"SUB" : "subtraction",
        #"DIV" : "division",
        #"POW" : "power"
    }
    path = directory + operations.get(operationType, "shit")
    fig = plt.figure()
    ax = plt.subplot(111)
    ax.set_title("Colour Map of decimal accuracy for " + operationType, fontsize = 10)
    ax.set_xlabel ('X Binary String', fontsize=12)
    ax.set_ylabel ('Y Binary String', fontsize=12)

    z = np.empty([256, 256], dtype=np.float64)
    for filename in glob.glob(os.path.join(path, '*.txt')):
        with open(os.path.join(os.getcwd(), filename), 'r') as f: # open in readonly mode
            #print("a intrat")
            if not(data_type in filename):
                continue
            print(filename)
            #data_type = os.path.basename(filename).split('.')[0]
            for row in f:
                row = row.split(' ')
                if(len(row) < 8):
                    continue
                # print(row[1].strip() == "NR")
                valX = int(row[0], 2)
                valY = int(row[1], 2)
                computed = row[4]
                #print(row[3])
                exact = row[5]
                computedDividExact = row[6]
                absoluteError = row[7]
                #print(row[0] + " DA:" + str(decimalAccuracy))
                #print(str(valX) + " " + str(valY))
                #if(valY == 256):
                #    print(row[1])
                #    print(BitArray(bin=row[1]))
                #    print(str(int.from_bytes(BitArray(bin=row[1]), byteorder='big', signed=False)))
                #    print(str(int.from_bytes(BitArray(bin=row[1]), byteorder='little', signed=False)))
                #    print(str(int(row[1], 2)))
                z[valX][valY] = decimalAccuracyFunction(computed, exact)
            #z = np.array(z)
            #z = z.astype(np.float64)
            max_value = np.nanmax(z[np.isfinite(z)])
            min_value = np.nanmin(z[np.isfinite(z)])
            print(str(max_value))
            print(str(min_value))
            print(np.count_nonzero(np.isposinf(z)))
            z[np.isposinf(z)] = 10
            #pcm = ax.pcolor(z, cmap='PuBu_r', shading='nearest')
            bounds = np.linspace(-4, 10, 50)
            norm = colors.TwoSlopeNorm(vmin=0, vcenter=5, vmax=10)
            z
            pcm = ax.pcolormesh(z, rasterized=True, norm=norm, cmap='binary', shading='auto')
            ax.set_xticks([0,128,256])
            ax.set_xticklabels(['00000000','10000000','11111111'])
            ax.set_yticks([0,128,256])
            ax.set_yticklabels(['00000000','10000000','11111111'])
            #ax.colorbar()
            #ax.matshow(z, cmap='ocean')
            #fig.colorbar(pcm, ax=ax, extend='max')
            #sns.kdeplot(ax=ax,data = y, cumulative = True, label = data_types[data_type], marker=markers[data_types[data_type]], linestyle=linestyles[data_types[data_type]], markersize=3, markevery=5)
            #ax.hist(y, bins=100, cumulative=True, label = data_type, 
            #        histtype='step', alpha=0.55, color='purple')

    print(z)
    #ax.legend()
    # Shrink current axis's height by 10% on the bottom
    #box = ax.get_position()
    #ax.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    #ax.set_xticks(range(-4, 10))
    fig.colorbar(pcm, ax=ax, orientation='vertical')

    # Put a legend below current axis
    #ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.2), fancybox=True, shadow=True, ncol=4, fontsize=8)
    image_format = 'pdf' # e.g .png, .svg, etc.
    image_name = operations.get(operationType, "shit") + "CMap.pdf"
    print("save image")
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)



def plotDoubleOperationDecimalAccuracyFile(operationType, data_type):
    data_types = {
        "FixedFloatingPoint_4_7_RE" : "FloatingPoint(4, 7)",
        "FixedP_12_6_RE" : "FixedP(6,6)",
        "IEEE754_4_7_RE" : "IEEE754(4, 7)",
        "Morris_3_12_RZ" : "Morris(3,12)",
        "MorrisHEB_3_12_RZ" : "MorrisHEB(3,12)",
        "MP_3_12_RE" : "MorrisBiasHEB(3,12)",
        "Posit_2_12_RE" : "Posit(12)",
        "PM_12_RE" : "MorrisUnaryHEB(12)"
    }
    directory = "results\\"
    operations = {
        "ADD" : "addition",
        "MUL" : "multiplication",
        "SUB" : "subtraction",
        "DIV" : "division",
        "POW" : "power"
    }
    path = directory + operations.get(operationType, "shit")

    z = np.empty([4096, 4096], dtype=np.float64)
    for filename in glob.glob(os.path.join(path, '*.txt')):
        with open(os.path.join(os.getcwd(), filename), 'r') as rf: # open in readonly mode
            print(filename)
            if not(data_type in filename):
                continue
            data_type = os.path.basename(filename).split('.')[0]
            for row in rf:
                row = row.split(' ')
                if(len(row) < 8):
                    continue
                valX = int(row[0], 2)
                valY = int(row[1], 2)
                computed = row[4]
                exact = row[5]
                computedDividExact = row[6]
                absoluteError = row[7]
                decimalAccuracy = 0
                if(computed.strip() == "NR"):
                    if(exact.strip() == "NR"):
                        decimalAccuracy = np.PINF
                    else:
                        decimalAccuracy = 0
                else:
                    if(exact.strip() == "NR"):
                        decimalAccuracy = 0
                    else:
                        computed_value = Decimal(computed)
                        exact_value = Decimal(exact)
                        if(exact_value == 0):
                            if(computed_value == 0):
                                decimalAccuracy = np.PINF
                            else:
                                decimalAccuracy = 0
                        else:
                            if(computed_value == exact_value):
                                decimalAccuracy = np.PINF
                            elif(computed_value == 0):
                                decimalAccuracy = 0
                            else:
                                decimalError = np.abs(np.log10(computed_value/exact_value))
                                decimalAccuracy = np.log10(1/decimalError)
                z[valX][valY] = decimalAccuracy
            max_value = np.nanmax(z[np.isfinite(z)])
            min_value = np.nanmin(z[np.isfinite(z)])
            print(str(max_value))
            print(str(min_value))
            with open(os.path.join(os.getcwd(), path + "\\" + data_type + "_DA.bin"), 'wb') as wf:
                np.save(wf, z)


def plotDoubleOperationColorMapFromDA(operationType, data_type):
    data_types = {
        "FixedFloatingPoint_4_7_RE" : "FloatingPoint(4, 7)",
        "FixedP_12_6_RE" : "FixedP(6,6)",
        "IEEE754_4_7_RE" : "IEEE754(4, 7)",
        "Morris_3_12_RZ" : "Morris(3,12)",
        "MorrisHEB_3_12_RZ" : "MorrisHEB(3,12)",
        "MP_3_12_RE" : "MorrisBiasHEB(3,12)",
        "Posit_2_12_RE" : "Posit(12)",
        "PM_12_RE" : "MorrisUnaryHEB(12)"
    }
    directory = "results\\"
    operations = {
        "ADD" : "addition",
        "MUL" : "multiplication",
        "SUB" : "subtraction",
        "DIV" : "division",
        "POW" : "power"
    }
    path = directory + operations.get(operationType, "shit")
    fig = plt.figure()
    ax = plt.subplot(111)
    ax.set_title("Colour Map of decimal accuracy for " + operationType, fontsize = 10)
    ax.set_xlabel ('X Binary String', fontsize=12)
    ax.set_ylabel ('Y Binary String', fontsize=12)

    z = np.empty([4096, 4096], dtype=np.float64)
    for filename in glob.glob(os.path.join(path, '*DA.bin')):
        if not(data_type in filename):
            continue
        print(filename)
        with open(os.path.join(os.getcwd(), filename), 'rb') as rf: # open in readonly mode
            data_type = os.path.basename(filename).split('_DA.')[0]
            z = np.load(rf)
            z[np.isposinf(z)] = 10
            #pcm = ax.pcolor(z, cmap='PuBu_r', shading='nearest')
            #bounds = np.linspace(-4, 10, 50)
            norm = colors.TwoSlopeNorm(vmin=0, vcenter=5, vmax=10)
            print(data_type)
            pcm = ax.pcolormesh(z, rasterized=True, norm=norm, cmap='binary', shading='auto')
            ax.set_xticks([0,2048,4095])
            ax.set_xticklabels(['000000000000','100000000000','111111111111'])
            ax.set_yticks([0,2048,4095])
            ax.set_yticklabels(['000000000000','100000000000','111111111111'])
            fig.colorbar(pcm, ax=ax, orientation='vertical')
    image_format = 'svg' # e.g .png, .svg, etc.
    image_name = operations.get(operationType, "shit") + "DACMap.svg"
    print("save image")
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def doubleOperationDecimalAccuracyFiles():
    data_types = {
        "FixedFloatingPoint_4_7_RE" : "FloatingPoint(4, 7)",
        "FixedP_12_6_RE" : "FixedP(6,6)",
        "IEEE754_4_7_RE" : "IEEE754(4, 7)",
        "Morris_3_12_RZ" : "Morris(3,12)",
        "MorrisHEB_3_12_RZ" : "MorrisHEB(3,12)",
        "MP_3_12_RE" : "MorrisBiasHEB(3,12)",
        "Posit_2_12_RE" : "Posit(12)",
        "PM_12_RE" : "MorrisUnaryHEB(12)"
    }
    directory = "results\\"
    operations = {
        "ADD" : "addition",
        "MUL" : "multiplication",
        "DIV" : "division"
    }
    z = np.empty([4096, 4096], dtype=np.float64)
    for operationType in operations:
        path = directory + operations.get(operationType, "shit")
        for filename in glob.glob(os.path.join(path, '*.txt')):
            with open(os.path.join(os.getcwd(), filename), 'r') as rf: # open in readonly mode
                print(filename)
                data_type = os.path.basename(filename).split('.')[0]
                if not(data_type in data_types):
                    continue
                for row in rf:
                    row = row.split(' ')
                    if(len(row) < 8):
                        continue
                    valX = int(row[0], 2)
                    valY = int(row[1], 2)
                    computed = row[4]
                    exact = row[5]
                    computedDividExact = row[6]
                    absoluteError = row[7]
                    z[valX][valY] = decimalAccuracyFunction(computed, exact)
                max_value = np.nanmax(z[np.isfinite(z)])
                min_value = np.nanmin(z[np.isfinite(z)])
                print(str(max_value))
                print(str(min_value))
                with open(os.path.join(os.getcwd(), path + "\\" + data_type + "_DA.bin"), 'wb') as wf:
                    np.save(wf, z)


def doubleOperationColorMapsFromDA():
    data_types = {
        "FixedFloatingPoint_4_7_RE" : "FloatingPoint(4, 7)",
        "FixedP_12_6_RE" : "FixedP(6,6)",
        "IEEE754_4_7_RE" : "IEEE754(4, 7)",
        "Morris_3_12_RZ" : "Morris(3,12)",
        "MorrisHEB_3_12_RZ" : "MorrisHEB(3,12)",
        "MP_3_12_RE" : "MorrisBiasHEB(3,12)",
        "Posit_2_12_RE" : "Posit(12)",
        "PM_12_RE" : "MorrisUnaryHEB(12)"
    }
    directory = "results\\"
    image_directory = "images\\"
    operations = {
        "ADD" : "addition",
        "MUL" : "multiplication",
        "DIV" : "division"
    }
    z = np.empty([4096, 4096], dtype=np.float64)
    image_format = 'svg' # e.g .png, .svg, etc.
    for operationType in operations:
        path = directory + operations.get(operationType, "shit")
        image_path = image_directory + operations.get(operationType, "shit")
        for filename in glob.glob(os.path.join(path, '*_DA.bin')):
            print(filename)
            data_type = os.path.basename(filename).split('_DA.')[0]
            if not(data_type in data_types):
                continue
            fig = plt.figure()
            ax = plt.subplot(111)
            ax.set_title("Colour Map of decimal accuracy for " + operationType, fontsize = 10)
            ax.set_xlabel ('X Binary String', fontsize=12)
            ax.set_ylabel ('Y Binary String', fontsize=12)
            with open(os.path.join(os.getcwd(), filename), 'rb') as rf: # open in readonly mode
                z = np.load(rf)
                z[np.isposinf(z)] = 10
                #pcm = ax.pcolor(z, cmap='PuBu_r', shading='nearest')
                #bounds = np.linspace(-4, 10, 50)
                norm = colors.TwoSlopeNorm(vmin=0, vcenter=5, vmax=10)
                print(data_type)
                pcm = ax.pcolormesh(z, rasterized=True, norm=norm, cmap='binary', shading='auto')
                ax.set_xticks([0,2048,4095])
                ax.set_xticklabels(['000000000000','100000000000','111111111111'])
                ax.set_yticks([0,2048,4095])
                ax.set_yticklabels(['000000000000','100000000000','111111111111'])
                fig.colorbar(pcm, ax=ax, orientation='vertical')
            image_name = image_path + "\\" + data_type + "_DA_CMap.svg"
            print("save image:" + image_name)
            plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def singleOperationDecimalAccuracyFiles():
    data_types = {
        "FixedFloatingPoint_5_10_RE" : "FixedFloatingPoint(5, 10, RE)",
        "FixedP_16_8_RE" : "FixedPoint(8, 8, RE)",
        "IEEE754_5_10_RE" : "IEEE754(5, 10, RE)",
        "Morris_4_16_RZ" : "Morris(16, 4, RZ)",
        "MorrisHEB_4_16_RZ" : "MorrisHEB(16, 4, RZ)",
        "MP_4_16_RE" : "MorrisBiasHEB(16, 4, RE)",
        "Posit_2_16_RE" : "Posit(16, 2, RE)",
        "PM_16_RE" : "MorrisUnaryHEB(16, RE)"
    }
    operations = {
        "SQRT" : "square_root",
        #"LN" : "natural_logarithm",
        "INV" : "inverse",
        "EXP" : "exponential",
        "SIN" : "sinus",
        #"SINH" : "hyperbolic_sinus",
        "CRT" : "cube_root"
    }
    directory = "results\\"
    y = np.empty([65536, 1], dtype=np.float64)
    for operationType in operations:
        path = directory + operations.get(operationType, "shit")
        for filename in glob.glob(os.path.join(path, '*.txt')):
            print(filename)
            data_type = os.path.basename(filename).split('.')[0]
            if not(data_type in data_types):
                continue
            with open(os.path.join(os.getcwd(), filename), 'r') as rf: # open in readonly mode
                for row in rf:
                    row = row.split(' ')
                    if(len(row) < 6):
                        continue
                    valX = int(row[0], 2)
                    computed = row[2]
                    exact = row[3]
                    computedDividExact = row[4]
                    absoluteError = row[5]
                    y[valX] = decimalAccuracyFunction(computed, exact)
                max_value = np.nanmax(y[np.isfinite(y)])
                min_value = np.nanmin(y[np.isfinite(y)])
                print(str(max_value))
                print(str(min_value))
                with open(os.path.join(os.getcwd(), path + "\\" + data_type + "_DA.bin"), 'wb') as wf:
                    np.save(wf, y)


def singleOperationCDFFromDA():
    data_types = {
        "FixedFloatingPoint_5_10_RE" : "FixedFloatingPoint(5, 10, RE)",
        "FixedP_16_8_RE" : "FixedPoint(8, 8, RE)",
        "IEEE754_5_10_RE" : "IEEE754(5, 10, RE)",
        "Morris_4_16_RZ" : "Morris(16, 4, RZ)",
        "MorrisHEB_4_16_RZ" : "MorrisHEB(16, 4, RZ)",
        "MP_4_16_RE" : "MorrisBiasHEB(16, 4, RE)",
        "Posit_2_16_RE" : "Posit(16, 2, RE)",
        "PM_16_RE" : "MorrisUnaryHEB(16, RE)"
    }
    markers = {
        "FixedFloatingPoint(5, 10, RE)" : "s",
        "FixedPoint(8, 8, RE)" : "o",
        "IEEE754(5, 10, RE)" : "v",
        "Morris(16, 4, RZ)" : "^",
        "MorrisHEB(16, 4, RZ)" : "x",
        "MorrisBiasHEB(16, 4, RE)" : "s",
        "Posit(16, 2, RE)" : "o",
        "MorrisUnaryHEB(16, RE)" : "v"
    }
    linestyles = {
        "FixedFloatingPoint(5, 10, RE)" : "-",
        "FixedPoint(8, 8, RE)" : "-",
        "IEEE754(5, 10, RE)" : "-",
        "Morris(16, 4, RZ)" : "-",
        "MorrisHEB(16, 4, RZ)" : "-",
        "MorrisBiasHEB(16, 4, RE)" : ":",
        "Posit(16, 2, RE)" : ":",
        "MorrisUnaryHEB(16, RE)" : ":"
    }
    operations = {
        "SQRT" : "square_root",
        "LN" : "natural_logarithm",
        "INV" : "inverse",
        "EXP" : "exponential",
        "SIN" : "sinus",
        "SINH" : "hyperbolic_sinus",
        "CRT" : "cube_root"
    }
    colors = {
        "FixedFloatingPoint(5, 10, RE)" : 'red',
        "FixedPoint(8, 8, RE)" :  'green',
        "IEEE754(5, 10, RE)" : 'blue',
        "Morris(16, 4, RZ)" : 'cyan',
        "MorrisHEB(16, 4, RZ)" : 'yellow',
        "MorrisBiasHEB(16, 4, RE)" : 'chocolate',
        "Posit(16, 2, RE)" : 'salmon',
        "MorrisUnaryHEB(16, RE)" : 'purple'
    }#['black', ,, , , , , , , 'lightgreen', 'gold']
    palettes = {
        "FixedFloatingPoint(5, 10, RE)" : 'Purples',
        "FixedPoint(8, 8, RE)" :  'Blues',
        "IEEE754(5, 10, RE)" : 'Greens',
        "Morris(16, 4, RZ)" : 'Oranges',
        "MorrisHEB(16, 4, RZ)" : 'Reds',
        "MorrisBiasHEB(16, 4, RE)" : 'Purples',
        "Posit(16, 2, RE)" : 'Blues',
        "MorrisUnaryHEB(16, RE)" : 'Reds'
    }

    directory = "results\\"
    image_directory = "images\\"
    y = np.empty([65536, 1], dtype=np.float64)
    image_format = 'svg' # e.g .png, .svg, etc.
    for operationType in operations:
        path = directory + operations.get(operationType, "shit")
        image_path = image_directory + operations.get(operationType, "shit")
        fig = plt.figure()
        ax = plt.subplot(111)
        ax.set_title("CDF of decimal accuracy for " + operationType, fontsize = 10)
        plt.xlabel('Decimal Accuracy', fontsize=8)
        for filename in glob.glob(os.path.join(path, '*_DA.bin')):
            print(filename)
            data_type = os.path.basename(filename).split('_DA.')[0]
            if not(data_type in data_types):
                continue
            with open(os.path.join(os.getcwd(), filename), 'rb') as rf: # open in readonly mode
                y = np.load(rf)
            y = y.astype(np.float64)
            # y = y[np.isfinite(y)]
            y[np.isposinf(y)] = 10
            y[np.isnan(y)] = 0
            y[y>10] = 10
            #  color=colors[data_types[data_type]],
            # y.squeeze()
            sns.kdeplot(ax=ax, data = y, cumulative = True, palette=palettes[data_types[data_type]], label = data_types[data_type], marker=markers[data_types[data_type]], linestyle=linestyles[data_types[data_type]], markersize=3, markevery=5)

        box = ax.get_position()
        ax.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
        ax.set_xticks(range(-2, 11))

        # Put a legend below current axis
        ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.2), fancybox=True, shadow=True, ncol=4, fontsize=8)
        image_name = image_path + "\\" + operationType + "_DA_CDF.svg"
        print("save image:" + image_name)
        plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)

def densityFiles():
    data_types = {
        "FixedFloatingPoint_5_10_RE" : "FixedFloatingPoint(5, 10, RE)",
        "FixedP_16_8_RE" : "FixedPoint(8, 8, RE)",
        "IEEE754_5_10_RE" : "IEEE754(5, 10, RE)",
        "Morris_4_16_RZ" : "Morris(16, 4, RZ)",
        "MorrisHEB_4_16_RZ" : "MorrisHEB(16, 4, RZ)",
        "MP_4_16_RE" : "MorrisBiasHEB(16, 4, RE)",
        "Posit_2_16_RE" : "Posit(16, 2, RE)",
        "PM_16_RE" : "MorrisUnaryHEB(16, RE)"
    }
    operations = {
        "SQRT" : "square_root",
        "LN" : "natural_logarithm",
        "INV" : "inverse",
        "EXP" : "exponential",
        "SIN" : "sinus",
        "SINH" : "hyperbolic_sinus",
        "CRT" : "cube_root"
    }
    operationType = "CRT"
    directory = "results\\"
    #y = np.empty([65536, 1], dtype=np.float64)
    y = np.empty(65536, dtype=np.dtype(Decimal))
    #for operationType in operations:
    path_read = directory + operations.get(operationType, "shit")
    path_write = directory + "density"
    for filename in glob.glob(os.path.join(path_read, '*.txt')):
        print(filename)
        data_type = os.path.basename(filename).split('.')[0]
        if not(data_type in data_types):
            continue
        with open(os.path.join(os.getcwd(), filename), 'r') as rf: # open in readonly mode
            for row in rf:
                row = row.split(' ')
                if(len(row) < 6):
                    continue
                valX = int(row[0], 2)
                if(row[1].strip() == "NR"):
                    #valY = np.PINF
                    valY = Decimal('+inf')
                else:
                    valY = Decimal(row[1])
                y[valX] = valY
            #max_value = np.nanmax(y[np.isfinite(y)])
            #min_value = np.nanmin(y[np.isfinite(y)])
            finite_y = np.fromiter((x for x in y if x.is_finite()), dtype=y.dtype)
            # Create an empty list
            #filter_arr = []

            # go through each element in arr
            #for element in y:
            #    # if the element is higher than 42, set the value to True, otherwise False:
            #    if element.is_finite():
            #        filter_arr.append(True)
            #    else:
            #        filter_arr.append(False)
            #finite_y = y[filter_arr]
            finite_y = np.abs(finite_y)
            max_value = np.nanmax(finite_y)
            min_value = np.nanmin(finite_y[finite_y!=0])
            print(str(max_value))
            print(str(min_value))
            print(np.log10(max_value/min_value))
            with open(os.path.join(os.getcwd(), path_write + "\\" + data_type + "_DA.bin"), 'wb') as wf:
                np.save(wf, y)

def densityPlot():
    data_types = {
        "FixedFloatingPoint_5_10_RE" : "FixedFloatingPoint(5, 10, RE)",
    #    "FixedP_16_8_RE" : "FixedPoint(8, 8, RE)",
        "IEEE754_5_10_RE" : "IEEE754(5, 10, RE)",
        "Morris_4_16_RZ" : "Morris(16, 4, RZ)",
        "MorrisHEB_4_16_RZ" : "MorrisHEB(16, 4, RZ)",
        "MP_4_16_RE" : "MorrisBiasHEB(16, 4, RE)",
        "Posit_2_16_RE" : "Posit(16, 2, RE)",
        "PM_16_RE" : "MorrisUnaryHEB(16, RE)"
    }
    markers = {
        "FixedFloatingPoint(5, 10, RE)" : "s",
        "FixedPoint(8, 8, RE)" : "o",
        "IEEE754(5, 10, RE)" : "v",
        "Morris(16, 4, RZ)" : "^",
        "MorrisHEB(16, 4, RZ)" : "x",
        "MorrisBiasHEB(16, 4, RE)" : "s",
        "Posit(16, 2, RE)" : "o",
        "MorrisUnaryHEB(16, RE)" : "v"
    }
    linestyles = {
        "FixedFloatingPoint(5, 10, RE)" : "-",
        "FixedPoint(8, 8, RE)" : "-",
        "IEEE754(5, 10, RE)" : "-",
        "Morris(16, 4, RZ)" : "-",
        "MorrisHEB(16, 4, RZ)" : "-",
        "MorrisBiasHEB(16, 4, RE)" : "-",
        "Posit(16, 2, RE)" : "-",
        "MorrisUnaryHEB(16, RE)" : "-"
    }
    operations = {
        "SQRT" : "square_root",
        "LN" : "natural_logarithm",
        "INV" : "inverse",
        "EXP" : "exponential",
        "SIN" : "sinus",
        "SINH" : "hyperbolic_sinus",
        "CRT" : "cube_root",
        "DNS" : "density"
    }
    colors = {
        "FixedFloatingPoint(5, 10, RE)" : 'purple',
        "FixedPoint(8, 8, RE)" :  'blue',
        "IEEE754(5, 10, RE)" : 'green',
        "Morris(16, 4, RZ)" : 'orange',
        "MorrisHEB(16, 4, RZ)" : 'lightgreen',
        "MorrisBiasHEB(16, 4, RE)" : 'cyan',
        "Posit(16, 2, RE)" : 'blue',
        "MorrisUnaryHEB(16, RE)" : 'red'
    }#['black', ,, , , , , , , 'lightgreen', 'gold']
    palettes = {
        "FixedFloatingPoint(5, 10, RE)" : 'Purples',
        "FixedPoint(8, 8, RE)" :  'Blues',
        "IEEE754(5, 10, RE)" : 'Greens',
        "Morris(16, 4, RZ)" : 'Oranges',
        "MorrisHEB(16, 4, RZ)" : 'Reds',
        "MorrisBiasHEB(16, 4, RE)" : 'Purples',
        "Posit(16, 2, RE)" : 'Blues',
        "MorrisUnaryHEB(16, RE)" : 'Reds'
    }

    directory = "results\\"
    image_directory = "images\\"
    #y = np.empty([65536, 1], dtype=np.float64)
    y = np.empty(65536, dtype=np.dtype(Decimal))
    image_format = 'pdf' # e.g .png, .svg, etc.
    #for operationType in operations:
    operationType = "DNS"
    path = directory + operations.get(operationType, "shit")
    image_path = image_directory + operations.get(operationType, "shit")
    fig = plt.figure()
    ax = plt.subplot(111)
    ax.set_xlim(-10,10)
    #ax.set_title("Density Plot", fontsize = 10)
    plt.xlabel('Log10(abs(X))', fontsize=8)
    for filename in glob.glob(os.path.join(path, '*_DA.bin')):
        print(filename)
        data_type = os.path.basename(filename).split('_DA.')[0]
        if not(data_type in data_types):
            continue
        with open(os.path.join(os.getcwd(), filename), 'rb') as rf: # open in readonly mode
            y = np.load(rf, allow_pickle=True)
        #y[y>=0] = np.log10(y[>=0])
        #y[y<0] = -np.log10(-y[<0])
        # y = y[np.isfinite(y)]
        #y[np.isposinf(y)] = 10
        #print(y[np.isnan(y)])
        #print(y[np.isposinf(y)])
        #print(y[np.isneginf(y)])
        #y = y[~np.isposinf(y)]
        #y = y[~np.isneginf(y)]
        #y = y[~np.isnan(y)]
        #y = y[y <= 2]
        #y = y[y >= -2]
        #print(y)
        #y[y>10] = 10
        #  color=colors[data_types[data_type]],
        # palette=palettes[data_types[data_type]],
        # y.squeeze()
        #y = np.unique(y)
        if data_type == "MorrisHEB_4_16_RZ":
            print("fara abs")
            print(len(y))
            print(len(np.unique(y)))
            #print(len(y[np.isposinf(y)] ))
            #print(len(y[np.isnan(y)]))
            print(len(y[y==0]))
            unq, unq_ind, unq_cnt = np.unique(y, return_inverse=True, return_counts=True)
            print(unq)
            print(len(unq_cnt))
            print(unq[unq_cnt>1])
        y = np.abs(y)
        finite_y = np.fromiter((x for x in y if x.is_finite()), dtype=y.dtype)
        if data_type == "MorrisHEB_4_16_RZ":
            print("cu abs")
            print(len(y))
            print(len(np.unique(y)))
            #print(len(y[np.isposinf(y)] ))
            #print(len(y[np.isnan(y)]))
            print(len(y[y==0]))
        y = np.unique(y)
        #y = y[~np.isposinf(y)] 
        #y = y[~np.isnan(y)]
        y = y[y!=0]
        y = np.log10(y)
        print(data_type)
        print(np.count_nonzero((y<3) & (y>-3)))
        print(np.count_nonzero((y>3) | (y<-3)))
        print(len(y))
        z = np.zeros([int(65536/2)-len(y), 1], dtype=np.float64)
        z[z==0] = np.PINF
        print(len(z))
        #y = np.concatenate((y,z), axis=None, dtype=np.float64)
        print(y.shape)
        print(len(y))
        w = y.astype(np.float64)
        #y[y>20]=20
        #y[y<-20]=-20
        #sns.kdeplot(ax=ax, data = y,clip=(-12,12), palette=palettes[data_types[data_type]], label = data_types[data_type], marker=markers[data_types[data_type]], linestyle=linestyles[data_types[data_type]], markersize=3, markevery=5)
        #sns.kdeplot(ax=ax, data = y.squeeze(),  label = data_types[data_type], marker=markers[data_types[data_type]], linestyle=linestyles[data_types[data_type]], markersize=3, markevery=5)
        kde_dict = {
            "marker" : markers[data_types[data_type]],
            "linestyle" : linestyles[data_types[data_type]],
            "markersize" : 3,
            "markevery" : 5,
            "label" : data_types[data_type]
        }
        kde_dict_p = {
            "bw_method" : "silverman"
        }
        binwidth = 0.1
        #, binwidth=binwidth, kwargs=kde_dict
        ad = sns.histplot(ax=ax, data = w, label = data_types[data_type], stat='count', element='poly', kde=False, fill=False, binwidth = 0.15, linestyle=linestyles[data_types[data_type]], marker=markers[data_types[data_type]], markersize=3, markevery=5, color=colors[data_types[data_type]])
        #ax2 = ax.twinx()
        #ax2.set_ylim(0, ax.get_ylim()[1] / binwidth)
        #ax2.yaxis.set_major_formatter(PercentFormatter(1 / binwidth))  # show axis such that 1/binwidth corresponds to 100%
        #sns.kdeplot(data=y,ax=ax2, label = data_types[data_type],color=colors[data_types[data_type]], marker=markers[data_types[data_type]], linestyle=linestyles[data_types[data_type]], markersize=3, markevery=5)
        #ad.containers[0].remove()

    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    #ax.set_xticks(range(-10, 10, 2))

    # Put a legend below current axis
    ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.2), fancybox=True, shadow=True, ncol=4, fontsize=8)
    image_name = image_path + "\\" + operationType + "_DA_CDF.pdf"
    print("save image:" + image_name)
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def doubleOperationFigures():
    data_types = {
        "FixedFloatingPoint_4_7_RE" : "FixedFloatingPoint(4, 7, RE)",
        "FixedP_12_6_RE" : "FixedPoint(6, 6, RE)",
        "IEEE754_4_7_RE" : "IEEE754(4, 7, RE)",
        "Morris_3_12_RZ" : "Morris(12, 3, RZ)",
        "MorrisHEB_3_12_RZ" : "MorrisHEB(12, 3, RZ)",
        "MP_3_12_RE" : "MorrisBiasHEB(12, 3, RE)",
        "Posit_2_12_RE" : "Posit(12, 2, RE)",
        "PM_12_RE" : "MorrisUnaryHEB(12, RE)"
    }
    directory = "results\\"
    image_directory = "images\\"
    operations = {
        "ADD" : "addition",
        "MUL" : "multiplication",
        "DIV" : "division"
    }
    z = np.empty([4096, 4096], dtype=np.float64)
    image_format = 'pdf' # e.g .png, .svg, etc.
    norm = colors.TwoSlopeNorm(vmin=0, vcenter=5, vmax=10)
    for operationType in operations:
        path = directory + operations.get(operationType, "shit")
        image_path = image_directory + operations.get(operationType, "shit")
        fig, ax = plt.subplots(2, 4, sharex=True, sharey=True, figsize=(20,10))
        index = 0
        for filename in glob.glob(os.path.join(path, '*_DA.bin')):
            print(filename)
            data_type = os.path.basename(filename).split('_DA.')[0]
            if not(data_type in data_types):
                continue
            with open(os.path.join(os.getcwd(), filename), 'rb') as rf: # open in readonly mode
                z = np.load(rf)
                davg = np.average(z[~np.isposinf(z)])
                exact = np.count_nonzero(np.isposinf(z))
                z[np.isposinf(z)] = 10
                #pcm = ax.pcolor(z, cmap='PuBu_r', shading='nearest')
                #bounds = np.linspace(-4, 10, 50)
                tavg = np.average(z)
                print(data_type)
                print(davg)
                print(exact)
                print(tavg)
                pcm = ax[int(index/4), index%4].pcolormesh(z, rasterized=True, norm=norm, cmap='binary', shading='auto')
            ax[int(index/4), index%4].set_title(data_types[data_type], fontsize = 10)
            index = index + 1
        # y-label
        ax[0, 0].set_yticks([0,2048,4095])
        ax[0, 0].set_yticklabels(['000000000000','100000000000','111111111111'], fontsize = 5)
        ax[1, 0].set_yticks([0,2048,4095])
        ax[1, 0].set_yticklabels(['000000000000','100000000000','111111111111'], fontsize = 5)
        # x-label
        ax[1, 0].set_xticks([0,2048,4095])
        ax[1, 0].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        ax[1, 1].set_xticks([0,2048,4095])
        ax[1, 1].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        ax[1, 2].set_xticks([0,2048,4095])
        ax[1, 2].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        ax[1, 3].set_xticks([0,2048,4095])
        ax[1, 3].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        #legend
        divider = make_axes_locatable(ax[0, 3])
        cax = divider.append_axes("right", size="5%", pad=0.05)
        fig.colorbar(pcm, cax=cax)
        divider = make_axes_locatable(ax[1, 3])
        cax = divider.append_axes("right", size="5%", pad=0.05)
        fig.colorbar(pcm, cax=cax)
        #fig.colorbar(pcm, ax=ax[1,4], orientation='vertical')
        image_name = image_path + "\\" + "FULL" + "_DA_CMap.pdf"
        print("save image:" + image_name)
        plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)

def singleOperationFigure():
    data_types = {
        "FixedFloatingPoint_5_10_RE" : "FixedFloatingPoint(5, 10, RE)",
        "FixedP_16_8_RE" : "FixedPoint(8, 8, RE)",
        "IEEE754_5_10_RE" : "IEEE754(5, 10, RE)",
        "Morris_4_16_RZ" : "Morris(16, 4, RZ)",
        "MorrisHEB_4_16_RZ" : "MorrisHEB(16, 4, RZ)",
        "MP_4_16_RE" : "MorrisBiasHEB(16, 4, RE)",
        "Posit_2_16_RE" : "Posit(16, 2, RE)",
        "PM_16_RE" : "MorrisUnaryHEB(16, RE)"
    }
    markers = {
        "FixedFloatingPoint(5, 10, RE)" : "s",
        "FixedPoint(8, 8, RE)" : "o",
        "IEEE754(5, 10, RE)" : "v",
        "Morris(16, 4, RZ)" : "^",
        "MorrisHEB(16, 4, RZ)" : "x",
        "MorrisBiasHEB(16, 4, RE)" : "s",
        "Posit(16, 2, RE)" : "o",
        "MorrisUnaryHEB(16, RE)" : "v"
    }
    linestyles = {
        "FixedFloatingPoint(5, 10, RE)" : "-",
        "FixedPoint(8, 8, RE)" : "-",
        "IEEE754(5, 10, RE)" : "-",
        "Morris(16, 4, RZ)" : "-",
        "MorrisHEB(16, 4, RZ)" : "-",
        "MorrisBiasHEB(16, 4, RE)" : ":",
        "Posit(16, 2, RE)" : ":",
        "MorrisUnaryHEB(16, RE)" : ":"
    }
    operations = {
        "SQRT" : "square_root",
        "LN" : "natural_logarithm",
        "INV" : "inverse",
        "EXP" : "exponential",
        "SIN" : "sinus",
        #"SINH" : "hyperbolic_sinus",
        "CRT" : "cube_root"
    }
    colors = {
        "FixedFloatingPoint(5, 10, RE)" : 'red',
        "FixedPoint(8, 8, RE)" :  'green',
        "IEEE754(5, 10, RE)" : 'blue',
        "Morris(16, 4, RZ)" : 'cyan',
        "MorrisHEB(16, 4, RZ)" : 'yellow',
        "MorrisBiasHEB(16, 4, RE)" : 'chocolate',
        "Posit(16, 2, RE)" : 'salmon',
        "MorrisUnaryHEB(16, RE)" : 'purple'
    }#['black', ,, , , , , , , 'lightgreen', 'gold']
    palettes = {
        "FixedFloatingPoint(5, 10, RE)" : 'Purples',
        "FixedPoint(8, 8, RE)" :  'Blues',
        "IEEE754(5, 10, RE)" : 'Greens',
        "Morris(16, 4, RZ)" : 'Oranges',
        "MorrisHEB(16, 4, RZ)" : 'Reds',
        "MorrisBiasHEB(16, 4, RE)" : 'Purples',
        "Posit(16, 2, RE)" : 'Blues',
        "MorrisUnaryHEB(16, RE)" : 'Reds'
    }
    titles = {
        "SQRT" : "$ \sqrt{x} $",
        "LN" : "$ \ln(x) $",
        "INV" : "$ x^{-1} $ ",
        "EXP" : "$ e^x $",
        "SIN" : "$ \sin(x) $",
        #"SINH" : "$ \sinh(x) $",
        "CRT" : "$ \sqrt[3]{x} $"
    }

    plt.rcParams.update({'font.size': 24})
    directory = "results\\"
    image_directory = "images\\"
    y = np.empty([65536, 1], dtype=np.float64)
    image_format = 'pdf' # e.g .png, .svg, etc.
    fig, ax = plt.subplots(2, 3, sharex=False, sharey=True, figsize=(34,20))
    index = 0
    image_path = image_directory + "single"
    for operationType in operations:
        path = directory + operations.get(operationType, "shit")
        ax[int(index/3), index%3].set_title(titles[operationType], fontsize = 26)
        for filename in glob.glob(os.path.join(path, '*_DA.bin')):
            print(filename)
            data_type = os.path.basename(filename).split('_DA.')[0]
            if not(data_type in data_types):
                continue
            with open(os.path.join(os.getcwd(), filename), 'rb') as rf: # open in readonly mode
                y = np.load(rf)
            y = y.astype(np.float64)
            # y = y[np.isfinite(y)]
            y[np.isposinf(y)] = 10
            y[np.isnan(y)] = 0
            y[y>10] = 10
            y[y<0] = 0.05
            #  color=colors[data_types[data_type]],
            # y.squeeze()
            sns.kdeplot(ax=ax[int(index/3), index%3], data = y, cumulative = True, palette=palettes[data_types[data_type]], label = data_types[data_type], marker=markers[data_types[data_type]], linestyle=linestyles[data_types[data_type]], markersize=5, markevery=5)
        if index == 0:
            legend = ax[0, 0].legend(loc='lower right', bbox_to_anchor=(0.5, -0.7), fancybox=True, shadow=True, ncol=4, fontsize=24)
            handles, labels = ax[0, 0].get_legend_handles_labels()
        else:
            legend = ax[int(index/3), index%3].legend()
        legend.remove()
        index = index + 1
    #box = ax.get_position()
    #ax.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    # x-label
    ax[1, 0].set_xticks(range(0, 11))
    ax[1, 1].set_xticks(range(0, 11))
    ax[1, 2].set_xticks(range(0, 11))
    ax[0, 0].set_xticks(range(0, 11))
    ax[0, 1].set_xticks(range(0, 11))
    ax[0, 2].set_xticks(range(0, 11))
    ax[1, 0].set_xlim([0, 11])
    ax[1, 1].set_xlim([0, 11])
    ax[1, 2].set_xlim([0, 11])
    ax[0, 0].set_xlim([0, 11])
    ax[0, 1].set_xlim([0, 11])
    ax[0, 2].set_xlim([0, 11])
    ax[1, 0].set_xlabel("Decimal Accuracy", fontsize = 26)
    ax[1, 1].set_xlabel("Decimal Accuracy", fontsize = 26)
    ax[1, 2].set_xlabel("Decimal Accuracy", fontsize = 26)
    # Put a legend below current axis
    #ax[0, 0].legend(loc='bottom center', bbox_to_anchor=(0.5, -0.5), fancybox=True, shadow=True, ncol=4, fontsize=20)
    #divider = make_axes_locatable(ax[1, 0])
    ##cax = divider.append_axes("bottom", size="15%", pad=0.05)
    fig.legend(handles, labels, loc='lower center', bbox_to_anchor=(0.5, -0.02), fancybox=True, shadow=True, ncol=4, fontsize=24)
    #cax.legend(handles, labels, loc='lower center', fancybox=True, shadow=True, ncol=4, fontsize=26)
    image_name = image_path + "\\" + "FULL_DA_CDF.pdf"
    print("save image:" + image_name)
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def MathematicalBenchmark():
    filename = "results\\math.txt"
    finalMatrix = np.zeros((14, 9), dtype='d')
    labels = []
    with open(os.path.join(os.getcwd(), filename), 'r') as rf:
        index = 0
        for row in rf:
            row = row.split(' ')
            labels.append(row[0])
            for jindex in range(1,10):
                finalMatrix[index, jindex-1] = float(row[jindex])
            index = index + 1
    #save
    with open('results\\math.npy', 'wb') as f:
        np.save(f, finalMatrix)
    
    with open('results\\math.npy', 'rb') as f:
        finalMatrix = np.load(f)
    # for the text one
    #np.savetxt('test.out', finalMatrix, delimiter=',')
    
    #the plot
    #z = np.empty([18, 9], dtype=np.float64)
    image_format = 'pdf' # e.g .png, .svg, etc.
    norm = colors.TwoSlopeNorm(vmin=0, vcenter=5, vmax=20)
    fig, ax = plt.subplots(1, 1, sharex=True, sharey=True, figsize=(20,10))
    data = finalMatrix
    pcm = ax.pcolormesh(data, rasterized=True, norm=norm, cmap='binary', shading='auto')
    # y-label
    #ax[0, 0].set_yticks([0,2048,4095])
    #ax[0, 0].set_yticklabels(['000000000000','100000000000','111111111111'], fontsize = 5)
    ax.set_yticks(np.array(range(0,14)) + 0.5)
    ax.set_yticklabels(labels, fontsize = 20)
    ax.set_xticks(np.array(range(0,9)) + 0.5)
    ax.set_xticklabels(["TT","EXP","FACT1","FACT2","h","NA","c","e","k"], fontsize = 20)
    ax.set_xlabel("Benchmarks", fontsize = 20)
    ax.set_ylabel("NRS", fontsize = 20)
    for nrs in range(0,14):
        for benchmark in range(0,9):
            ax.text(benchmark + 0.5, nrs + 0.5, '{:0.1f}'.format(data[nrs, benchmark]), ha='center', va='center',
            bbox=dict(boxstyle='round', facecolor='white', edgecolor='0.3'))
    #legend
    divider = make_axes_locatable(ax)
    cax = divider.append_axes("right", size="5%", pad=0.05)
    cbar = fig.colorbar(pcm, cax=cax)
    #cbar.ax.set_ylabel("Decimnal Accuracy", rotation=270, fontsize = 20)
    cax.set_ylabel("Decimnal Accuracy", rotation=270, fontsize = 20)
    cax.set_yticks([0,5,20])
    cax.set_yticklabels([0,5,20], fontsize = 20)

    #fig.colorbar(pcm, ax=ax[1,4], orientation='vertical')
    image_name = "images\\math.pdf"
    print("save image:" + image_name)
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def customDoubleOperation():
    data_types = {
        #"FixedFloatingPoint_4_7_RE" : "FixedFloatingPoint(4, 7, RE)",
        #"FixedP_12_6_RE" : "FixedPoint(6, 6, RE)",
        #"IEEE754_4_7_RE" : "IEEE754(4, 7, RE)",
        "Morris_3_12_RZ" : "Morris(12, 3, RZ)",
        "MorrisHEB_3_12_RZ" : "MorrisHEB(12, 3, RZ)"#,
        #"MP_3_12_RE" : "MorrisBiasHEB(12, 3, RE)",
        #"Posit_2_12_RE" : "Posit(12, 2, RE)",
        #"PM_12_RE" : "MorrisUnaryHEB(12, RE)"
    }
    directory = "results\\"
    image_directory = "images\\"
    operations = {
        "ADD" : "addition"#,
        #"MUL" : "multiplication",
        #"DIV" : "division"
    }
    z = np.empty([4096, 4096], dtype=np.float64)
    image_format = 'pdf' # e.g .png, .svg, etc.
    norm = colors.TwoSlopeNorm(vmin=0, vcenter=5, vmax=10)
    for operationType in operations:
        path = directory + operations.get(operationType, "shit")
        image_path = image_directory + operations.get(operationType, "shit")
        #fig, ax = plt.subplots(2, 4, sharex=True, sharey=True, figsize=(20,10))
        #FixedFloatPoint single
        #fig, ax = plt.subplots(1, 1, sharex=True, sharey=True, figsize=(10,10))
        #rest of them
        fig, ax = plt.subplots(1, 2, sharex=True, sharey=True, figsize=(20,10))
        index = 0
        for data_type in data_types:
        #for filename in glob.glob(os.path.join(path, '*_DA.bin')):
        #    print(filename)
        #    data_type = os.path.basename(filename).split('_DA.')[0]
        #    if not(data_type in data_types):
        #        continue
            filename = data_type + "_DA.bin"
            print(filename)
            with open(os.path.join(path, filename), 'rb') as rf: # open in readonly mode
                z = np.load(rf)
                davg = np.average(z[~np.isposinf(z)])
                exact = np.count_nonzero(np.isposinf(z))
                z[np.isposinf(z)] = 10
                #pcm = ax.pcolor(z, cmap='PuBu_r', shading='nearest')
                #bounds = np.linspace(-4, 10, 50)
                tavg = np.average(z)
                print(data_type)
                print(davg)
                print(exact)
                print(tavg)
                #pcm = ax[int(index/4), index%4].pcolormesh(z, rasterized=True, norm=norm, cmap='binary', shading='auto')
                #FixedFloatPoint single
                #pcm = ax.pcolormesh(z, rasterized=True, norm=norm, cmap='binary', shading='auto')
                #rest of them
                pcm = ax[index%2].pcolormesh(z, rasterized=True, norm=norm, cmap='binary', shading='auto')
            #ax[int(index/4), index%4].set_title(data_types[data_type], fontsize = 10)
            #rest of them
            ax[index%2].set_title(data_types[data_type], fontsize = 10)
            index = index + 1
        # y-label
        #ax[0, 0].set_yticks([0,2048,4095])
        #ax[0, 0].set_yticklabels(['000000000000','100000000000','111111111111'], fontsize = 5)
        #ax[1, 0].set_yticks([0,2048,4095])
        #ax[1, 0].set_yticklabels(['000000000000','100000000000','111111111111'], fontsize = 5)
        #FixedFloatPoint single
        #ax.set_yticks([0,2048,4095])
        #ax.set_yticklabels(['000000000000','100000000000','111111111111'], fontsize = 5)
        #rest of them
        ax[0].set_yticks([0,2048,4095])
        ax[0].set_yticklabels(['000000000000','100000000000','111111111111'], fontsize = 10)
        
        # x-label
        #ax[1, 0].set_xticks([0,2048,4095])
        #ax[1, 0].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        #ax[1, 1].set_xticks([0,2048,4095])
        #ax[1, 1].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        #ax[1, 2].set_xticks([0,2048,4095])
        #ax[1, 2].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        #ax[1, 3].set_xticks([0,2048,4095])
        #ax[1, 3].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        #FixedFloatPoint single
        #ax.set_xticks([0,2048,4095])
        #ax.set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 5)
        #rest of them
        ax[0].set_xticks([0,2048,4095])
        ax[0].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 10)
        ax[1].set_xticks([0,2048,4095])
        ax[1].set_xticklabels(['000000000000','100000000000','111111111111'], rotation=45, fontsize = 10)



        #legend
        #divider = make_axes_locatable(ax[0, 3])
        #cax = divider.append_axes("right", size="5%", pad=0.05)
        #fig.colorbar(pcm, cax=cax)
        #divider = make_axes_locatable(ax[1, 3])
        #cax = divider.append_axes("right", size="5%", pad=0.05)
        #fig.colorbar(pcm, cax=cax)
        #fig.colorbar(pcm, ax=ax[1,4], orientation='vertical')
        #FixedFloatPoint single
        #divider = make_axes_locatable(ax)
        #cax = divider.append_axes("right", size="5%", pad=0.05)
        #fig.colorbar(pcm, cax=cax)
        #rest of them
        divider = make_axes_locatable(ax[1])
        cax = divider.append_axes("right", size="5%", pad=0.05)
        fig.colorbar(pcm, cax=cax)
        
        
        image_name = image_path + "\\" + "MorrisMorrisHEB" + "ADD_CMap.pdf"
        print("save image:" + image_name)
        plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)
#plotSingleOperation("SQRT")
#plotSingleOperationCDF("INV")
#plotDoubleOperationColorMap("ADD", "Posit_1_8_RE")
#plotDoubleOperationDecimalAccuracyFile("ADD", "IEEE754_3_4_RE")
#plotDoubleOperationColorMapFromDA("ADD", "IEEE754_3_4_RE")
#doubleOperationDecimalAccuracyFiles()
#singleOperationDecimalAccuracyFiles()
#doubleOperationColorMapsFromDA()
#singleOperationCDFFromDA()
#densityFiles()
#densityPlot()
#doubleOperationFigures()
#singleOperationFigure()
#MathematicalBenchmark()
customDoubleOperation()