package com.hx.log.log.interf;

import com.hx.json.JSONObject;

import java.io.OutputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * SimpleLogger
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/16/2017 7:09 PM
 */
public interface Logger {

    /**
     * 配置out输出文件
     *
     * @param logFile 给定的文件的路径
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:13 PM
     * @since 1.0
     */
    void setOutLogFile(String logFile) throws Exception;

    /**
     * 配置err输出文件
     *
     * @param logFile 给定的文件的路径
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:13 PM
     * @since 1.0
     */
    void setErrLogFile(String logFile) throws Exception;

    /**
     * 配置 out 输出的mode
     *
     * @param mode 给定的名称
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:14 PM
     * @since 1.0
     */
    void setOutMode(String mode);

    /**
     * 配置 err 输出的mode
     *
     * @param mode 给定的名称
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:14 PM
     * @since 1.0
     */
    void setErrMode(String mode);
    /**
     * 获取输出模式
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/15/2017 8:42 PM
     * @since 1.0
     */
    String getMode(int modeIdx);

    /**
     * 获取标准输出模式
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/15/2017 8:42 PM
     * @since 1.0
     */
    String getOutMode();

    /**
     * 获取错误输出模式
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/15/2017 8:42 PM
     * @since 1.0
     */
    String getErrMode();

    /**
     * 配置 out 输出流
     *
     * @param stream 给定的输出流
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:14 PM
     * @since 1.0
     */
    void setOutStream(OutputStream stream);

    /**
     * 配置 err 输出流
     *
     * @param stream 给定的输出流
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:14 PM
     * @since 1.0
     */
    void setErrStream(OutputStream stream);

    /**
     * 配置 out 的是否输出到文件, 以及输出文件的路径
     *
     * @param outToLogFile 是否输出到文件
     * @param logFile      输出文件的路径
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:15 PM
     * @since 1.0
     */
    void setOutToLogFile(boolean outToLogFile, String logFile) throws Exception;

    /**
     * 配置 out 的是否输出到文件
     *
     * @param outToLogFile 是否输出到文件
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:15 PM
     * @since 1.0
     */
    void setOutToLogFile(boolean outToLogFile) throws Exception;

    /**
     * 配置 err 的是否输出到文件, 以及输出文件的路径
     *
     * @param errToLogFile 是否输出到文件
     * @param logFile      输出文件的路径
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:15 PM
     * @since 1.0
     */
    void setErrToLogFile(boolean errToLogFile, String logFile) throws Exception;

    /**
     * 配置 err 的是否输出到文件, 以及输出文件的路径
     *
     * @param errToLogFile 是否输出到文件
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:15 PM
     * @since 1.0
     */
    void setErrToLogFile(boolean errToLogFile) throws Exception;

    /**
     * 控制给定的字符串的输出
     *
     * @param modeIdx  给定的输出模式的idx
     * @param logStr   给定的输出的内容
     * @param isFormat 是否使用logPattern格式化
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:18 PM
     * @since 1.0
     */
    void dispathLogInfo(int modeIdx, String logStr, boolean isFormat);

    void dispathLogInfo(int modeIdx, String logStr);

    /**
     * 将给定的字符串向给定的输出模式输出信息
     *
     * @param str        给定的字符串
     * @param appendCRLF 是否需要添加crlf
     * @param isFormat   是否使用logPattern格式化
     * @param modeIdx    输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:20 PM
     * @since 1.0
     */
    void log(String str, boolean appendCRLF, boolean isFormat, int modeIdx);

    /**
     * 打印字符串, 对象, 按照给定的pattern填充数据
     *
     * @param value 给定的bool值
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:19 PM
     * @since 1.0
     */
    void log(boolean value);

    void log();

    void log(String str, boolean appendCRLF, int modeIdx);

    void log(String str, boolean appendCRLF);

    void log(String obj);

    void log(Object obj, boolean appendCRLF);

    void log(Object obj);

    void logf(String pattern, Object[] args, boolean appendCRLF);

    void logf(String pattern, Object... args);

    /**
     * 如果需要格式化的话, 格式化需要打印的数据
     *
     * @param content    给定的字符串
     * @param appendCRLF 是否需要添加crlf
     * @param isFormat   是否使用logPattern格式化
     * @param modeIdx    输出模式的索引
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 6:21 PM
     * @since 1.0
     */
    String logLogPatternFormat(String content, boolean appendCRLF, boolean isFormat, int modeIdx);

    String logLogPatternFormat(String content, boolean appendCRLF);

    String logLogPatternFormat(String content);

    /**
     * 打印迭代器中的数据
     *
     * @param it         给定的迭代器
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式额索引
     * @param appendCRLF 是否需要添加crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:22 PM
     * @since 1.0
     */
    <T> void log(Iterator<T> it, String sep, int modeIdx, boolean appendCRLF);

    <T> void log(Iterator<T> it, String sep, boolean appendCRLF);

    <T> void log(Iterator<T> it, boolean appendCRLF);

    <T> void log(Iterator<T> it);

    <T> void log(Iterator<T> it, String sep);

    <T> void log(List<T> list, String sep, boolean appendCRLF);

    <T> void log(List<T> list, boolean appendCRLF);

    <T> void log(List<T> list);

    <T> void log(List<T> list, String sep);

    <T> void log(Set<T> set, String sep, boolean appendCRLF);

    <T> void log(Set<T> set, boolean appendCRLF);

    <T> void log(Set<T> set);

    <T> void log(Set<T> set, String sep);

    /**
     * 打印给定的map
     *
     * @param map        给定的map
     * @param kvSep      kv之间的分隔符
     * @param sep        entry之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否添加crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:23 PM
     * @since 1.0
     */
    <K, V> void log(Map<K, V> map, String kvSep, String sep, int modeIdx, boolean appendCRLF);

    <K, V> void log(Map<K, V> map, String sep, int modeIdx, boolean appendCRLF);

    <K, V> void log(Map<K, V> map, String kvSep, String sep, boolean appendCRLF);

    <K, V> void log(Map<K, V> map, String kvSep, String sep);

    <K, V> void log(Map<K, V> map, String sep, boolean appendCRLF);

    <K, V> void log(Map<K, V> map, boolean appendCRLF);

    <K, V> void log(Map<K, V> map);

    <K, V> void log(Map<K, V> map, String sep);

    /**
     * 打印boolean[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    void log(boolean[] arr, String sep, int modeIdx, boolean appendCRLF);

    void log(boolean[] ls, String sep, boolean appendCRLF);

    void log(boolean[] ls, boolean appendCRLF);

    void log(boolean[] ls);

    void log(boolean[] ls, String sep);

    /**
     * 打印byte[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    void log(byte[] arr, String sep, int modeIdx, boolean appendCRLF);

    void log(byte[] ls, String sep, boolean appendCRLF);

    void log(byte[] ls, boolean appendCRLF);

    void log(byte[] ls);

    void log(byte[] ls, String sep);

    /**
     * 打印char[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    void log(char[] arr, String sep, int modeIdx, boolean appendCRLF);

    void log(char[] ls, String sep, boolean appendCRLF);

    void log(char[] ls, boolean appendCRLF);

    void log(char[] ls);

    void log(char[] ls, String sep);

    /**
     * 打印int[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    void log(int[] arr, String sep, int modeIdx, boolean appendCRLF);

    void log(int[] ls, String sep, boolean appendCRLF);

    void log(int[] ls, boolean appendCRLF);

    void log(int[] ls);

    void log(int[] ls, String sep);

    /**
     * 打印long[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    void log(long[] arr, String sep, int modeIdx, boolean appendCRLF);

    void log(long[] ls, String sep, boolean appendCRLF);

    void log(long[] ls, boolean appendCRLF);

    void log(long[] ls);

    void log(long[] ls, String sep);

    /**
     * 打印double[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    void log(float[] arr, String sep, int modeIdx, boolean appendCRLF);

    void log(float[] ls, String sep, boolean appendCRLF);

    void log(float[] ls, boolean appendCRLF);

    void log(float[] ls);

    void log(float[] ls, String sep);

    /**
     * 打印double[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    void log(double[] arr, String sep, int modeIdx, boolean appendCRLF);

    void log(double[] ls, String sep, boolean appendCRLF);

    void log(double[] ls, boolean appendCRLF);

    void log(double[] ls);

    void log(double[] ls, String sep);

    /**
     * 打印T[]
     *
     * @param arr        给定的数组
     * @param sep        元素之间的分隔符
     * @param modeIdx    输出模式索引
     * @param appendCRLF 是否输出crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:24 PM
     * @since 1.0
     */
    <T> void log(T[] arr, String sep, int modeIdx, boolean appendCRLF);

    <T> void log(T[] ls, String sep, boolean appendCRLF);

    <T> void log(T[] ls, boolean appendCRLF);

    <T> void log(T[] ls);

    <T> void log(T[] ls, String sep);

    void log(boolean[][] arr, String sep, int modeIdx);

    void log(boolean[][] arr, String sep);

    void log(boolean[][] arr);

    void log(byte[][] arr, String sep, int modeIdx);

    void log(byte[][] arr, String sep);

    void log(byte[][] arr);

    void log(char[][] arr, String sep, int modeIdx);

    void log(char[][] arr, String sep);

    void log(char[][] arr);

    void log(int[][] arr, String sep, int modeIdx);

    void log(int[][] arr, String sep);

    void log(int[][] arr);

    void log(long[][] arr, String sep, int modeIdx);

    void log(long[][] arr, String sep);

    void log(long[][] arr);

    void log(float[][] arr, String sep);

    void log(float[][] arr);

    void log(float[][] arr, String sep, int modeIdx);

    void log(double[][] arr, String sep);

    void log(double[][] arr);

    void log(double[][] arr, String sep, int modeIdx);

    <T> void log(T[][] arr, String sep, int modeIdx);

    <T> void log(T[][] arr, String sep);

    <T> void log(T[][] arr);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    void log(boolean[] arr, Iterator<Integer> it, String sep, int modeIdx);

    void log(boolean[] arr, Iterator<Integer> it, String sep);

    void log(boolean[] arr, Iterator<Integer> it);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    void log(byte[] arr, Iterator<Integer> it, String sep, int modeIdx);

    void log(byte[] arr, Iterator<Integer> it, String sep);

    void log(byte[] arr, Iterator<Integer> it);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    void log(char[] arr, Iterator<Integer> it, String sep, int modeIdx);

    void log(char[] arr, Iterator<Integer> it, String sep);

    void log(char[] arr, Iterator<Integer> it);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    void log(int[] arr, Iterator<Integer> it, String sep, int modeIdx);

    void log(int[] arr, Iterator<Integer> it, String sep);

    void log(int[] arr, Iterator<Integer> it);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    void log(long[] arr, Iterator<Integer> it, String sep, int modeIdx);

    void log(long[] arr, Iterator<Integer> it, String sep);

    void log(long[] arr, Iterator<Integer> it);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    void log(float[] arr, Iterator<Integer> it, String sep, int modeIdx);

    void log(float[] arr, Iterator<Integer> it, String sep);

    void log(float[] arr, Iterator<Integer> it);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    void log(double[] arr, Iterator<Integer> it, String sep, int modeIdx);

    void log(double[] arr, Iterator<Integer> it, String sep);

    void log(double[] arr, Iterator<Integer> it);

    /**
     * 按照it迭代出来的索引 依次输出arr中对应位置的数据
     *
     * @param arr     给定的数组
     * @param it      迭代索引的迭代器
     * @param sep     各个元素之间的分隔符
     * @param modeIdx 输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:39 PM
     * @since 1.0
     */
    <T> void log(T[] arr, Iterator<Integer> it, String sep, int modeIdx);

    <T> void log(T[] arr, Iterator<Integer> it, String sep);

    <T> void log(T[] arr, Iterator<Integer> it);

    /**
     * 格式化给定的字符串, 然后 输出
     *
     * @param logPattern 给定的pattern
     * @param argsMap    参数
     * @param modeIdx    输出模式
     * @return void
     * @author Jerry.X.He
     * @date 5/15/2017 7:48 PM
     * @since 1.0
     */
    void log(String logPattern, JSONObject argsMap, int modeIdx);

    void log(String logPattern, JSONObject argsMap);

    /**
     * 格式化给定的字符串, 然后 输出
     *
     * @param logPattern 给定的pattern
     * @param args       参数
     * @param modeIdx    输出模式
     * @return void
     * @author Jerry.X.He
     * @date 5/15/2017 7:48 PM
     * @since 1.0
     */
    void log(String logPattern, Object[] args, int modeIdx);

    <T> void log(String logPattern, Object... args);

    /**
     * 格式化给定的字符串, 然后 输出
     *
     * @param logPattern 给定的pattern
     * @param args       参数
     * @param modeIdx    输出模式
     * @return void
     * @author Jerry.X.He
     * @date 5/15/2017 7:48 PM
     * @since 1.0
     */
    void logWithIdx(String logPattern, Object[] args, int modeIdx);

    <T> void logWithIdx(String logPattern, Object... args);

    /**
     * 输出给定的两个值
     *
     * @param bool01 值1
     * @param bool02 值2
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:44 PM
     * @since 1.0
     */
    void log(boolean bool01, boolean bool02);

    void log(byte row, byte col);

    void log(char row, char col);

    void log(int row, int col);

    void log(long row, long col);

    void log(float row, float col);

    void log(double row, double col);

    <T1, T2> void log(T1 row, T2 col);

    /**
     * 打印一条水平线
     *
     * @param n       需要输出的水平线的数量
     * @param modeIdx 输出模式
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:46 PM
     * @since 1.0
     */
    void logHorizon(int n, int modeIdx);

    void logHorizon(int n);

    void logHorizon();

    /**
     * 键入一个/ n个回车
     *
     * @param n       需要输出的水平线的数量
     * @param modeIdx 输出模式
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:46 PM
     * @since 1.0
     */
    void logEnter(int n, int modeIdx);

    void logEnter();

    void logEnter(int n);

    // ----------------------------- seps ----------------------------------------

    /**
     * error 部分
     */
    void err(boolean appendCRLF);

    void err();

    void err(String str, boolean appendCRLF);

    void err(String obj);

    void err(Object obj, boolean appendCRLF);

    void err(Object obj);

    void errf(String pattern, Object[] args, boolean appendCRLF);

    void errf(String pattern, Object... args);

    String errLogPatternFormat(String content, boolean appendCRLF);

    String errLogPatternFormat(String content);

    <T> void err(Iterator<T> it, String sep, boolean appendCRLF);

    <T> void err(Iterator<T> it, boolean appendCRLF);

    <T> void err(Iterator<T> it);

    <T> void err(Iterator<T> it, String sep);

    <T> void err(List<T> list, String sep, boolean appendCRLF);

    <T> void err(List<T> list, boolean appendCRLF);

    <T> void err(List<T> list);

    <T> void err(List<T> list, String sep);

    <T> void err(Set<T> set, String sep, boolean appendCRLF);

    <T> void err(Set<T> set, boolean appendCRLF);

    <T> void err(Set<T> set);

    <T> void err(Set<T> set, String sep);

    <K, V> void err(Map<K, V> map, String kvSep, String sep, boolean appendCRLF);

    <K, V> void err(Map<K, V> map, String kvSep, String sep);

    <K, V> void err(Map<K, V> map, String sep, boolean appendCRLF);

    <K, V> void err(Map<K, V> map, boolean appendCRLF);

    <K, V> void err(Map<K, V> map);

    <K, V> void err(Map<K, V> map, String sep);

    void err(boolean[] arr, String sep, boolean appendCRLF);

    void err(boolean[] arr, boolean appendCRLF);

    void err(boolean[] arr);

    void err(boolean[] arr, String sep);

    void err(byte[] arr, String sep, boolean appendCRLF);

    void err(byte[] arr, boolean appendCRLF);

    void err(byte[] arr);

    void err(byte[] arr, String sep);

    void err(char[] arr, String sep, boolean appendCRLF);

    void err(char[] arr, boolean appendCRLF);

    void err(char[] arr);

    void err(char[] arr, String sep);

    void err(int[] arr, String sep, boolean appendCRLF);

    void err(int[] arr, boolean appendCRLF);

    void err(int[] arr);

    void err(int[] arr, String sep);

    void err(long[] arr, String sep, boolean appendCRLF);

    void err(long[] arr, boolean appendCRLF);

    void err(long[] arr);

    void err(long[] arr, String sep);

    void err(float[] arr, String sep, boolean appendCRLF);

    void err(float[] arr, boolean appendCRLF);

    void err(float[] arr);

    void err(float[] arr, String sep);

    void err(double[] arr, String sep, boolean appendCRLF);

    void err(double[] arr, boolean appendCRLF);

    void err(double[] arr);

    void err(double[] arr, String sep);

    <T> void err(T[] arr, String sep, boolean appendCRLF);

    <T> void err(T[] arr, boolean appendCRLF);

    <T> void err(T[] arr);

    <T> void err(T[] arr, String sep);

    void err(boolean[][] arr, String sep);

    void err(boolean[][] arr);

    void err(byte[][] arr, String sep);

    void err(byte[][] arr);

    void err(char[][] arr, String sep);

    void err(char[][] arr);

    void err(int[][] arr, String sep);

    void err(int[][] arr);

    void err(long[][] arr, String sep);

    void err(long[][] arr);

    void err(float[][] arr, String sep);

    void err(float[][] arr);

    void err(double[][] arr, String sep);

    void err(double[][] arr);

    <T> void err(T[][] arr, String sep);

    <T> void err(T[][] arr);

    void err(boolean[] arr, Iterator<Integer> it, String sep);

    void err(boolean[] arr, Iterator<Integer> it);

    void err(byte[] arr, Iterator<Integer> it, String sep);

    void err(byte[] arr, Iterator<Integer> it);

    void err(char[] arr, Iterator<Integer> it, String sep);

    void err(char[] arr, Iterator<Integer> it);

    void err(int[] arr, Iterator<Integer> it, String sep);

    void err(int[] arr, Iterator<Integer> it);

    void err(long[] arr, Iterator<Integer> it, String sep);

    void err(long[] arr, Iterator<Integer> it);

    void err(float[] arr, Iterator<Integer> it, String sep);

    void err(float[] arr, Iterator<Integer> it);

    void err(double[] arr, Iterator<Integer> it, String sep);

    void err(double[] arr, Iterator<Integer> it);

    <T> void err(T[] arr, Iterator<Integer> it, String sep);

    <T> void err(T[] arr, Iterator<Integer> it);

    void err(String logPattern, JSONObject argsMap);

    <T> void err(String logPattern, Object... args);

    <T> void errWithIdx(String logPattern, Object... args);

    void err(boolean bool01, boolean bool02);

    void err(byte row, byte col);

    void err(char row, char col);

    void err(int row, int col);

    void err(long row, long col);

    void err(float row, float col);

    void err(double row, double col);

    <T1, T2> void err(T1 row, T2 col);

    void errHorizon(int n);

    void errHorizon();

    void errEnter();

    void errEnter(int n);

    /**
     * 刷出缓冲区的数据		add at 2016.04.15
     *
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:49 PM
     * @since 1.0
     */
    void flush();

    /**
     * 获取当前已经缓冲的字符的数量, add at 2017.05.06
     *
     * @return long the size of chars buffered
     * @author Jerry.X.He
     * @date 5/6/2017 3:50 PM
     * @since 1.0
     */
    long size(int modeIdx);

    long sizeOfOut();

    long sizeOfErr();

}
