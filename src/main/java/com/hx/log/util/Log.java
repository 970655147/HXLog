/**
 * file name : Log.java
 * created at : 8:10:53 PM Apr 22, 2015
 * created by 970655147
 */
package com.hx.log.util;

import com.hx.json.JSONObject;
import com.hx.log.log.LogLevel;
import com.hx.log.log.SimpleLogger;
import com.hx.log.log.interf.Logger;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 打印数据相关的工具类
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 7:25 PM
 */
public final class Log {

    // disable constructor
    private Log() {
        Tools.assert0("can't instantiate !");
    }

    // 将所有的业务委托给Log.log		add at 2016.05.30
    /**
     * logger
     */
    public static Logger log = new SimpleLogger();
    /**
     * info-fatal logger
     */
    public static Logger infoFatalLogger = new SimpleLogger();
    /**
     * debug-warn logger
     */
    public static Logger debugWarnLogger = new SimpleLogger();
    /**
     * 最小的日志输出级别
     */
    public static LogLevel LOG_LEVEL_MIN = LogLevel.of(Constants.optString("logLevelMin"));

    static {
        try {
            infoFatalLogger.setOutToLogFile(false);
            infoFatalLogger.setErrToLogFile(false);
            debugWarnLogger.setOutToLogFile(false);
            debugWarnLogger.setErrToLogFile(false);
        } catch (Exception e) {
            e.printStackTrace();
        }
        infoFatalLogger.setOutMode("INFO");
        infoFatalLogger.setErrMode("FATAL");
        debugWarnLogger.setOutMode("DEBUG");
        debugWarnLogger.setErrMode("WARNNING");
        debugWarnLogger.setErrStream(System.out);
    }

    // --------------------------- 配置可配置变量的接口 ----------------------------------------
    public static void setOutLogFile(String logFile) throws Exception {
        log.setOutLogFile(logFile);
    }

    public static void setErrLogFile(String logFile) throws Exception {
        log.setErrLogFile(logFile);
    }

    public static void setInfoLogFile(String logFile) throws Exception {
        infoFatalLogger.setOutLogFile(logFile);
    }

    public static void setFatalLogFile(String logFile) throws Exception {
        infoFatalLogger.setErrLogFile(logFile);
    }

    public static void setDebugLogFile(String logFile) throws Exception {
        debugWarnLogger.setOutLogFile(logFile);
    }

    public static void setWarnLogFile(String logFile) throws Exception {
        debugWarnLogger.setErrLogFile(logFile);
    }

    public static void setOutToLogFile(boolean outToLogFile, String logFile) throws Exception {
        log.setOutToLogFile(outToLogFile, logFile);
    }

    public static void setOutToLogFile(boolean outToLogFile) throws Exception {
        log.setOutToLogFile(outToLogFile);
    }

    public static void setErrToLogFile(boolean errToLogFile, String logFile) throws Exception {
        log.setErrToLogFile(errToLogFile, logFile);
    }

    public static void setErrToLogFile(boolean errToLogFile) throws Exception {
        log.setErrToLogFile(errToLogFile);
    }

    public static void setInfoToLogFile(boolean outToLogFile, String logFile) throws Exception {
        infoFatalLogger.setOutToLogFile(outToLogFile, logFile);
    }

    public static void setInfoToLogFile(boolean outToLogFile) throws Exception {
        infoFatalLogger.setOutToLogFile(outToLogFile);
    }

    public static void setFatalToLogFile(boolean errToLogFile, String logFile) throws Exception {
        infoFatalLogger.setErrToLogFile(errToLogFile, logFile);
    }

    public static void setFatalToLogFile(boolean errToLogFile) throws Exception {
        infoFatalLogger.setErrToLogFile(errToLogFile);
    }

    public static void setDebugToLogFile(boolean outToLogFile, String logFile) throws Exception {
        debugWarnLogger.setOutToLogFile(outToLogFile, logFile);
    }

    public static void setDebugToLogFile(boolean outToLogFile) throws Exception {
        debugWarnLogger.setOutToLogFile(outToLogFile);
    }

    public static void setWarnToLogFile(boolean errToLogFile, String logFile) throws Exception {
        debugWarnLogger.setErrToLogFile(errToLogFile, logFile);
    }

    public static void setWarnToLogFile(boolean errToLogFile) throws Exception {
        debugWarnLogger.setErrToLogFile(errToLogFile);
    }

    // --------------------------- 业务方法 ----------------------------------------
    // 标准输出
    // 打印字符串, 对象, 按照给定的pattern填充数据
    // add at 2016.07.02
    public static void dispathLogInfo(int modeIdx, String logStr) {
        log.dispathLogInfo(modeIdx, logStr);
    }

    public static void dispathLogInfo(int modeIdx, String logStr, boolean isFormat) {
        log.dispathLogInfo(modeIdx, logStr, isFormat);
    }

    public static void infoDispathLogInfo(int modeIdx, String logStr) {
        infoFatalLogger.dispathLogInfo(modeIdx, logStr);
    }

    public static void infoDispathLogInfo(int modeIdx, String logStr, boolean isFormat) {
        infoFatalLogger.dispathLogInfo(modeIdx, logStr, isFormat);
    }

    public static void debugDispathLogInfo(int modeIdx, String logStr) {
        debugWarnLogger.dispathLogInfo(modeIdx, logStr);
    }

    public static void debugDispathLogInfo(int modeIdx, String logStr, boolean isFormat) {
        debugWarnLogger.dispathLogInfo(modeIdx, logStr, isFormat);
    }

    // log 相关
    public static void log() {
        log.log();
    }

    public static void log(String str, boolean appendCRLF) {
        log.log(str, appendCRLF);
    }

    public static void log(String str, boolean appendCRLF, int modeIdx) {
        log.log(str, appendCRLF, modeIdx);
    }

    public static void log(boolean appendCRLF) {
        log.log(appendCRLF);
    }

    public static void log(String obj) {
        log.log(obj);
    }

    public static void log(Object obj, boolean appendCRLF) {
        log.log(obj, appendCRLF);
    }

    public static void log(Object obj) {
        log.log(obj);
    }

    public static void logf(String pattern, Object[] args, boolean appendCRLF) {
        log.logf(pattern, args, appendCRLF);
    }

    public static void logf(String pattern, Object... args) {
        log.logf(pattern, args);
    }

    public String logLogPatternFormat(String content, boolean appendCRLF, boolean isFormat, int modeIdx) {
        return log.logLogPatternFormat(content, appendCRLF, isFormat, modeIdx);
    }

    public static String logLogPatternFormat(String content, boolean appendCRLF) {
        return log.logLogPatternFormat(content, appendCRLF);
    }

    public static String logLogPatternFormat(String content) {
        return log.logLogPatternFormat(content);
    }

    public static <T> void log(Iterator<T> it, String sep, boolean appendCRLF) {
        log.log(it, sep, appendCRLF);
    }

    public static <T> void log(Iterator<T> it, boolean appendCRLF) {
        log.log(it, appendCRLF);
    }

    public static <T> void log(Iterator<T> it) {
        log.log(it);
    }

    public static <T> void log(Iterator<T> it, String sep) {
        log.log(it, sep);
    }

    public static <T> void log(Iterator<T> it, String sep, int modeIdx, boolean appendCRLF) {
        log.log(it, sep, modeIdx, appendCRLF);
    }

    public static <T> void log(List<T> list, String sep, boolean appendCRLF) {
        log.log(list, sep, appendCRLF);
    }

    public static <T> void log(List<T> list, boolean appendCRLF) {
        log.log(list, appendCRLF);
    }

    public static <T> void log(List<T> list) {
        log.log(list);
    }

    public static <T> void log(List<T> list, String sep) {
        log.log(list, sep);
    }

    public static <T> void log(Set<T> set, String sep, boolean appendCRLF) {
        log.log(set, sep, appendCRLF);
    }

    public static <T> void log(Set<T> set, boolean appendCRLF) {
        log.log(set, appendCRLF);
    }

    public static <T> void log(Set<T> set) {
        log.log(set);
    }

    public static <T> void log(Set<T> set, String sep) {
        log.log(set, sep);
    }

    public static <K, V> void log(Map<K, V> map, String sep, int modeIdx, boolean appendCRLF) {
        log.log(map, sep, modeIdx, appendCRLF);
    }

    public static <K, V> void log(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        log.log(map, kvSep, sep, appendCRLF);
    }

    public static <K, V> void log(Map<K, V> map, String kvSep, String sep) {
        log.log(map, kvSep, sep);
    }

    public static <K, V> void log(Map<K, V> map, String sep, boolean appendCRLF) {
        log.log(map, sep, appendCRLF);
    }

    public static <K, V> void log(Map<K, V> map, boolean appendCRLF) {
        log.log(map, appendCRLF);
    }

    public static <K, V> void log(Map<K, V> map) {
        log.log(map);
    }

    public static <K, V> void log(Map<K, V> map, String sep) {
        log.log(map, sep);
    }

    public static <K, V> void log(Map<K, V> map, String kvSep, String sep, int modeIdx, boolean appendCRLF) {
        log.log(map, kvSep, sep, modeIdx, appendCRLF);
    }

    public static void log(int[] ls, String sep, boolean appendCRLF) {
        log.log(ls, sep, appendCRLF);
    }

    public static void log(int[] ls, boolean appendCRLF) {
        log.log(ls, appendCRLF);
    }

    public static void log(int[] ls) {
        log.log(ls);
    }

    public static void log(int[] ls, String sep) {
        log.log(ls, sep);
    }

    public static void log(int[] arr, String sep, int modeIdx, boolean appendCRLF) {
        log.log(arr, sep, modeIdx, appendCRLF);
    }

    public static void log(long[] ls, String sep, boolean appendCRLF) {
        log.log(ls, sep, appendCRLF);
    }

    public static void log(long[] ls, boolean appendCRLF) {
        log.log(ls, appendCRLF);
    }

    public static void log(long[] ls) {
        log.log(ls);
    }

    public static void log(long[] ls, String sep) {
        log.log(ls, sep);
    }

    public static void log(long[] arr, String sep, int modeIdx, boolean appendCRLF) {
        log.log(arr, sep, modeIdx, appendCRLF);
    }

    public static void log(double[] ls, String sep, boolean appendCRLF) {
        log.log(ls, sep, appendCRLF);
    }

    public static void log(double[] ls, boolean appendCRLF) {
        log.log(ls, appendCRLF);
    }

    public static void log(double[] ls) {
        log.log(ls);
    }

    public static void log(double[] ls, String sep) {
        log.log(ls, sep);
    }

    public static void log(double[] arr, String sep, int modeIdx, boolean appendCRLF) {
        log.log(arr, sep, modeIdx, appendCRLF);
    }

    public static void log(char[] ls, String sep, boolean appendCRLF) {
        log.log(ls, sep, appendCRLF);
    }

    public static void log(char[] ls, boolean appendCRLF) {
        log.log(ls, appendCRLF);
    }

    public static void log(char[] ls) {
        log.log(ls);
    }

    public static void log(char[] ls, String sep) {
        log.log(ls, sep);
    }

    public static void log(char[] arr, String sep, int modeIdx, boolean appendCRLF) {
        log.log(arr, sep, modeIdx, appendCRLF);
    }

    public static void log(byte[] ls, String sep, boolean appendCRLF) {
        log.log(ls, sep, appendCRLF);
    }

    public static void log(byte[] ls, boolean appendCRLF) {
        log.log(ls, appendCRLF);
    }

    public static void log(byte[] ls) {
        log.log(ls);
    }

    public static void log(byte[] ls, String sep) {
        log.log(ls, sep);
    }

    public static void log(byte[] arr, String sep, int modeIdx, boolean appendCRLF) {
        log.log(arr, sep, modeIdx, appendCRLF);
    }

    public static void log(boolean[] ls, String sep, boolean appendCRLF) {
        log.log(ls, sep, appendCRLF);
    }

    public static void log(boolean[] ls, boolean appendCRLF) {
        log.log(ls, appendCRLF);
    }

    public static void log(boolean[] ls) {
        log.log(ls);
    }

    public static void log(boolean[] ls, String sep) {
        log.log(ls, sep);
    }

    public static void log(boolean[] arr, String sep, int modeIdx, boolean appendCRLF) {
        log.log(arr, sep, modeIdx, appendCRLF);
    }

    public static <T> void log(T[] ls, String sep, boolean appendCRLF) {
        log.log(ls, sep, appendCRLF);
    }

    public static <T> void log(T[] ls, boolean appendCRLF) {
        log.log(ls, appendCRLF);
    }

    public static <T> void log(T[] ls) {
        log.log(ls);
    }

    public static <T> void log(T[] ls, String sep) {
        log.log(ls, sep);
    }

    public static <T> void log(T[] arr, String sep, int modeIdx, boolean appendCRLF) {
        log.log(arr, sep, modeIdx, appendCRLF);
    }

    public static void log(int[][] arr, String sep) {
        log.log(arr, sep);
    }

    public static void log(int[][] arr) {
        log.log(arr);
    }

    public static void log(int[][] arr, String sep, int modeIdx) {
        log.log(arr, sep, modeIdx);
    }

    public static void log(long[][] arr, String sep) {
        log.log(arr, sep);
    }

    public static void log(long[][] arr) {
        log.log(arr);
    }

    public static void log(long[][] arr, String sep, int modeIdx) {
        log.log(arr, sep, modeIdx);
    }

    public static void log(double[][] arr, String sep) {
        log.log(arr, sep);
    }

    public static void log(double[][] arr) {
        log.log(arr);
    }

    public static void log(double[][] arr, String sep, int modeIdx) {
        log.log(arr, sep, modeIdx);
    }

    public static void log(char[][] arr, String sep) {
        log.log(arr, sep);
    }

    public static void log(char[][] arr) {
        log.log(arr);
    }

    public static void log(char[][] arr, String sep, int modeIdx) {
        log.log(arr, sep, modeIdx);
    }

    public static void log(byte[][] arr, String sep) {
        log.log(arr, sep);
    }

    public static void log(byte[][] arr) {
        log.log(arr);
    }

    public static void log(byte[][] arr, String sep, int modeIdx) {
        log.log(arr, sep, modeIdx);
    }

    public static void log(boolean[][] arr, String sep) {
        log.log(arr, sep);
    }

    public static void log(boolean[][] arr) {
        log.log(arr);
    }

    public static void log(boolean[][] arr, String sep, int modeIdx) {
        log.log(arr, sep, modeIdx);
    }

    public static <T> void log(T[][] arr, String sep) {
        log.log(arr, sep);
    }

    public static <T> void log(T[][] arr) {
        log.log(arr);
    }

    public static <T> void log(T[][] arr, String sep, int modeIdx) {
        log.log(arr, sep, modeIdx);
    }

    public static void log(int[] arr, Iterator<Integer> it, String sep) {
        log.log(arr, it, sep);
    }

    public static void log(int[] arr, Iterator<Integer> it) {
        log.log(arr, it);
    }

    public static void log(int[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        log.log(arr, it, sep, modeIdx);
    }

    public static void log(long[] arr, Iterator<Integer> it, String sep) {
        log.log(arr, it, sep);
    }

    public static void log(long[] arr, Iterator<Integer> it) {
        log.log(arr, it);
    }

    public static void log(long[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        log.log(arr, it, sep, modeIdx);
    }

    public static void log(double[] arr, Iterator<Integer> it, String sep) {
        log.log(arr, it, sep);
    }

    public static void log(double[] arr, Iterator<Integer> it) {
        log.log(arr, it);
    }

    public static void log(double[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        log.log(arr, it, sep, modeIdx);
    }

    public static void log(char[] arr, Iterator<Integer> it, String sep) {
        log.log(arr, it, sep);
    }

    public static void log(char[] arr, Iterator<Integer> it) {
        log.log(arr, it);
    }

    public static void log(char[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        log.log(arr, it, sep, modeIdx);
    }

    public static void log(byte[] arr, Iterator<Integer> it, String sep) {
        log.log(arr, it, sep);
    }

    public static void log(byte[] arr, Iterator<Integer> it) {
        log.log(arr, it);
    }

    public static void log(byte[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        log.log(arr, it, sep, modeIdx);
    }

    public static void log(boolean[] arr, Iterator<Integer> it, String sep) {
        log.log(arr, it, sep);
    }

    public static void log(boolean[] arr, Iterator<Integer> it) {
        log.log(arr, it);
    }

    public static void log(boolean[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        log.log(arr, it, sep, modeIdx);
    }

    public static <T> void log(T[] arr, Iterator<Integer> it, String sep) {
        log.log(arr, it, sep);
    }

    public static <T> void log(T[] arr, Iterator<Integer> it) {
        log.log(arr, it);
    }

    public static <T> void log(T[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        log.log(arr, it, sep, modeIdx);
    }

    public static void log(String logPattern, JSONObject argsMap, int modeIdx) {
        log.log(logPattern, argsMap, modeIdx);
    }

    public static void log(String logPattern, JSONObject argsMap) {
        log.log(logPattern, argsMap);
    }

    public static void log(String logPattern, Object[] args, int modeIdx) {
        log.log(logPattern, args, modeIdx);
    }

    public static void log(String logPattern, Object... args) {
        log.log(logPattern, args);
    }

    public static void logWithIdx(String logPattern, Object[] args, int modeIdx) {
        log.logWithIdx(logPattern, args, modeIdx);
    }

    public static void logWithIdx(String logPattern, Object... args) {
        log.logWithIdx(logPattern, args);
    }

    public static void log(int row, int col) {
        log.log(row, col);
    }

    public static void log(long row, long col) {
        log.log(row, col);
    }

    public static void log(double row, double col) {
        log.log(row, col);
    }

    public static void log(char row, char col) {
        log.log(row, col);
    }

    public static void log(byte row, byte col) {
        log.log(row, col);
    }

    public static void log(boolean bool01, boolean bool02) {
        log.log(bool01, bool02);
    }

    public static <T1, T2> void log(T1 row, T2 col) {
        log.log(row, col);
    }

    public static void logHorizon(int n) {
        log.logHorizon(n);
    }

    public static void logHorizon() {
        log.logHorizon();
    }

    public static void logHorizon(int n, int modeIdx) {
        log.logHorizon(n, modeIdx);
    }

    public static void logEnter() {
        log.logEnter();
    }

    public static void logEnter(int n) {
        log.logEnter(n);
    }

    public static void logEnter(int n, int modeIdx) {
        log.logEnter(n, modeIdx);
    }

    // ----------------------------- seps ----------------------------------------

    // err相关
    public static void err(boolean appendCRLF) {
        log.err(appendCRLF);
    }

    public static void err() {
        log.err();
    }

    public static void err(String str, boolean appendCRLF) {
        log.err(str, appendCRLF);
    }

    public static void err(String obj) {
        log.err(obj);
    }

    public static void err(Object obj, boolean appendCRLF) {
        log.err(obj, appendCRLF);
    }

    public static void err(Object obj) {
        log.err(obj);
    }

    public static void errf(String pattern, Object[] args, boolean appendCRLF) {
        log.errf(pattern, args, appendCRLF);
    }

    public static void errf(String pattern, Object... args) {
        log.errf(pattern, args);
    }

    public static String errLogPatternFormat(String content, boolean appendCRLF) {
        return log.errLogPatternFormat(content, appendCRLF);
    }

    public static String errLogPatternFormat(String content) {
        return log.errLogPatternFormat(content);
    }

    public static <T> void err(Iterator<T> it, String sep, boolean appendCRLF) {
        log.err(it, sep, appendCRLF);
    }

    public static <T> void err(Iterator<T> it, boolean appendCRLF) {
        log.err(it, appendCRLF);
    }

    public static <T> void err(Iterator<T> it) {
        log.err(it);
    }

    public static <T> void err(Iterator<T> it, String sep) {
        log.err(it, sep);
    }

    public static <T> void err(List<T> list, String sep, boolean appendCRLF) {
        log.err(list, sep, appendCRLF);
    }

    public static <T> void err(List<T> list, boolean appendCRLF) {
        log.err(list, appendCRLF);
    }

    public static <T> void err(List<T> list) {
        log.err(list);
    }

    public static <T> void err(List<T> list, String sep) {
        log.err(list, sep);
    }

    public static <T> void err(Set<T> set, String sep, boolean appendCRLF) {
        log.err(set, sep, appendCRLF);
    }

    public static <T> void err(Set<T> set, boolean appendCRLF) {
        log.err(set, appendCRLF);
    }

    public static <T> void err(Set<T> set) {
        log.err(set);
    }

    public static <T> void err(Set<T> set, String sep) {
        log.err(set, sep);
    }

    public static <K, V> void err(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        log.err(map, kvSep, sep, appendCRLF);
    }

    public static <K, V> void err(Map<K, V> map, String kvSep, String sep) {
        log.err(map, kvSep, sep);
    }

    public static <K, V> void err(Map<K, V> map, String sep, boolean appendCRLF) {
        log.err(map, sep, appendCRLF);
    }

    public static <K, V> void err(Map<K, V> map, boolean appendCRLF) {
        log.err(map, appendCRLF);
    }

    public static <K, V> void err(Map<K, V> map) {
        log.err(map);
    }

    public static <K, V> void err(Map<K, V> map, String sep) {
        log.err(map, sep);
    }

    public static void err(int[] arr, String sep, boolean appendCRLF) {
        log.err(arr, sep, appendCRLF);
    }

    public static void err(int[] arr, boolean appendCRLF) {
        log.err(arr, appendCRLF);
    }

    public static void err(int[] arr) {
        log.err(arr);
    }

    public static void err(int[] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(long[] arr, String sep, boolean appendCRLF) {
        log.err(arr, sep, appendCRLF);
    }

    public static void err(long[] arr, boolean appendCRLF) {
        log.err(arr, appendCRLF);
    }

    public static void err(long[] arr) {
        log.err(arr);
    }

    public static void err(long[] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(double[] arr, String sep, boolean appendCRLF) {
        log.err(arr, sep, appendCRLF);
    }

    public static void err(double[] arr, boolean appendCRLF) {
        log.err(arr, appendCRLF);
    }

    public static void err(double[] arr) {
        log.err(arr);
    }

    public static void err(double[] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(byte[] arr, String sep, boolean appendCRLF) {
        log.err(arr, sep, appendCRLF);
    }

    public static void err(byte[] arr, boolean appendCRLF) {
        log.err(arr, appendCRLF);
    }

    public static void err(byte[] arr) {
        log.err(arr);
    }

    public static void err(byte[] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(char[] arr, String sep, boolean appendCRLF) {
        log.err(arr, sep, appendCRLF);
    }

    public static void err(char[] arr, boolean appendCRLF) {
        log.err(arr, appendCRLF);
    }

    public static void err(char[] arr) {
        log.err(arr);
    }

    public static void err(char[] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(boolean[] arr, String sep, boolean appendCRLF) {
        log.err(arr, sep, appendCRLF);
    }

    public static void err(boolean[] arr, boolean appendCRLF) {
        log.err(arr, appendCRLF);
    }

    public static void err(boolean[] arr) {
        log.err(arr);
    }

    public static void err(boolean[] arr, String sep) {
        log.err(arr, sep);
    }

    public static <T> void err(T[] arr, String sep, boolean appendCRLF) {
        log.err(arr, sep, appendCRLF);
    }

    public static <T> void err(T[] arr, boolean appendCRLF) {
        log.err(arr, appendCRLF);
    }

    public static <T> void err(T[] arr) {
        log.err(arr);
    }

    public static <T> void err(T[] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(int[][] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(int[][] arr) {
        log.err(arr);
    }

    public static void err(long[][] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(long[][] arr) {
        log.err(arr);
    }

    public static void err(double[][] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(double[][] arr) {
        log.err(arr);
    }

    public static void err(byte[][] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(byte[][] arr) {
        log.err(arr);
    }

    public static void err(char[][] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(char[][] arr) {
        log.err(arr);
    }

    public static void err(boolean[][] arr, String sep) {
        log.err(arr, sep);
    }

    public static void err(boolean[][] arr) {
        log.err(arr);
    }

    public static <T> void err(T[][] arr, String sep) {
        log.err(arr, sep);
    }

    public static <T> void err(T[][] arr) {
        log.err(arr);
    }

    public static void err(int[] arr, Iterator<Integer> it, String sep) {
        log.err(arr, it, sep);
    }

    public static void err(int[] arr, Iterator<Integer> it) {
        log.err(arr, it);
    }

    public static void err(long[] arr, Iterator<Integer> it, String sep) {
        log.err(arr, it, sep);
    }

    public static void err(long[] arr, Iterator<Integer> it) {
        log.err(arr, it);
    }

    public static void err(double[] arr, Iterator<Integer> it, String sep) {
        log.err(arr, it, sep);
    }

    public static void err(double[] arr, Iterator<Integer> it) {
        log.err(arr, it);
    }

    public static void err(char[] arr, Iterator<Integer> it, String sep) {
        log.err(arr, it, sep);
    }

    public static void err(char[] arr, Iterator<Integer> it) {
        log.err(arr, it);
    }

    public static void err(boolean[] arr, Iterator<Integer> it, String sep) {
        log.err(arr, it, sep);
    }

    public static void err(boolean[] arr, Iterator<Integer> it) {
        log.err(arr, it);
    }

    public static <T> void err(T[] arr, Iterator<Integer> it, String sep) {
        log.err(arr, it, sep);
    }

    public static <T> void err(T[] arr, Iterator<Integer> it) {
        log.err(arr, it);
    }

    public static void err(String logPattern, JSONObject argsMap) {
        log.err(logPattern, argsMap);
    }

    public static void err(String logPattern, Object... args) {
        log.err(logPattern, args);
    }

    public static void errWithIdx(String logPattern, Object... args) {
        log.errWithIdx(logPattern, args);
    }

    public static void err(int row, int col) {
        log.err(row, col);
    }

    public static void err(long row, long col) {
        log.err(row, col);
    }

    public static void err(double row, double col) {
        log.err(row, col);
    }

    public static void err(char row, char col) {
        log.err(row, col);
    }

    public static void err(byte row, byte col) {
        log.err(row, col);
    }

    public static void err(boolean bool01, boolean bool02) {
        log.err(bool01, bool02);
    }

    public static <T1, T2> void err(T1 row, T2 col) {
        log.err(row, col);
    }

    public static void errHorizon(int n) {
        log.errHorizon(n);
    }

    public static void errHorizon() {
        log.errHorizon();
    }

    public static void errEnter() {
        log.errEnter();
    }

    public static void errEnter(int n) {
        log.errEnter(n);
    }

    // ----------------------------- seps ----------------------------------------
    // add at 2016.10.15
    // info相关
    public static void info(boolean appendCRLF) {
        infoFatalLogger.log(appendCRLF);
    }

    public static void info() {
        infoFatalLogger.log();
    }

    public static void info(String str, boolean appendCRLF) {
        infoFatalLogger.log(str, appendCRLF);
    }

    public static void info(String obj) {
        infoFatalLogger.log(obj);
    }

    public static void info(Object obj, boolean appendCRLF) {
        infoFatalLogger.log(obj, appendCRLF);
    }

    public static void info(Object obj) {
        infoFatalLogger.log(obj);
    }

    public static void infof(String pattern, Object[] args, boolean appendCRLF) {
        infoFatalLogger.logf(pattern, args, appendCRLF);
    }

    public static void infof(String pattern, Object... args) {
        infoFatalLogger.logf(pattern, args);
    }

    public String infoLogPatternFormat(String content, boolean appendCRLF, boolean isFormat, int modeIdx) {
        return infoFatalLogger.logLogPatternFormat(content, appendCRLF, isFormat, modeIdx);
    }

    public static String infoLogPatternFormat(String content, boolean appendCRLF) {
        return infoFatalLogger.logLogPatternFormat(content, appendCRLF);
    }

    public static String infoLogPatternFormat(String content) {
        return infoFatalLogger.logLogPatternFormat(content);
    }

    public static <T> void info(Iterator<T> it, String sep, boolean appendCRLF) {
        infoFatalLogger.log(it, sep, appendCRLF);
    }

    public static <T> void info(Iterator<T> it, boolean appendCRLF) {
        infoFatalLogger.log(it, appendCRLF);
    }

    public static <T> void info(Iterator<T> it) {
        infoFatalLogger.log(it);
    }

    public static <T> void info(Iterator<T> it, String sep) {
        infoFatalLogger.log(it, sep);
    }

    public static <T> void info(List<T> list, String sep, boolean appendCRLF) {
        infoFatalLogger.log(list, sep, appendCRLF);
    }

    public static <T> void info(List<T> list, boolean appendCRLF) {
        infoFatalLogger.log(list, appendCRLF);
    }

    public static <T> void info(List<T> list) {
        infoFatalLogger.log(list);
    }

    public static <T> void info(List<T> list, String sep) {
        infoFatalLogger.log(list, sep);
    }

    public static <T> void info(Set<T> set, String sep, boolean appendCRLF) {
        infoFatalLogger.log(set, sep, appendCRLF);
    }

    public static <T> void info(Set<T> set, boolean appendCRLF) {
        infoFatalLogger.log(set, appendCRLF);
    }

    public static <T> void info(Set<T> set) {
        infoFatalLogger.log(set);
    }

    public static <T> void info(Set<T> set, String sep) {
        infoFatalLogger.log(set, sep);
    }

    public static <K, V> void info(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        infoFatalLogger.log(map, kvSep, sep, appendCRLF);
    }

    public static <K, V> void info(Map<K, V> map, String kvSep, String sep) {
        infoFatalLogger.log(map, kvSep, sep);
    }

    public static <K, V> void info(Map<K, V> map, String sep, boolean appendCRLF) {
        infoFatalLogger.log(map, sep, appendCRLF);
    }

    public static <K, V> void info(Map<K, V> map, boolean appendCRLF) {
        infoFatalLogger.log(map, appendCRLF);
    }

    public static <K, V> void info(Map<K, V> map) {
        infoFatalLogger.log(map);
    }

    public static <K, V> void info(Map<K, V> map, String sep) {
        infoFatalLogger.log(map, sep);
    }

    public static void info(int[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.log(arr, sep, appendCRLF);
    }

    public static void info(int[] arr, boolean appendCRLF) {
        infoFatalLogger.log(arr, appendCRLF);
    }

    public static void info(int[] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(int[] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(long[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.log(arr, sep, appendCRLF);
    }

    public static void info(long[] arr, boolean appendCRLF) {
        infoFatalLogger.log(arr, appendCRLF);
    }

    public static void info(long[] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(long[] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(double[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.log(arr, sep, appendCRLF);
    }

    public static void info(double[] arr, boolean appendCRLF) {
        infoFatalLogger.log(arr, appendCRLF);
    }

    public static void info(double[] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(double[] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(byte[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.log(arr, sep, appendCRLF);
    }

    public static void info(byte[] arr, boolean appendCRLF) {
        infoFatalLogger.log(arr, appendCRLF);
    }

    public static void info(byte[] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(byte[] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(char[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.log(arr, sep, appendCRLF);
    }

    public static void info(char[] arr, boolean appendCRLF) {
        infoFatalLogger.log(arr, appendCRLF);
    }

    public static void info(char[] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(char[] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(boolean[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.log(arr, sep, appendCRLF);
    }

    public static void info(boolean[] arr, boolean appendCRLF) {
        infoFatalLogger.log(arr, appendCRLF);
    }

    public static void info(boolean[] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(boolean[] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static <T> void info(T[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.log(arr, sep, appendCRLF);
    }

    public static <T> void info(T[] arr, boolean appendCRLF) {
        infoFatalLogger.log(arr, appendCRLF);
    }

    public static <T> void info(T[] arr) {
        infoFatalLogger.log(arr);
    }

    public static <T> void info(T[] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(int[][] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(int[][] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(long[][] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(long[][] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(double[][] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(double[][] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(byte[][] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(byte[][] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(char[][] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(char[][] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(boolean[][] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static void info(boolean[][] arr) {
        infoFatalLogger.log(arr);
    }

    public static <T> void info(T[][] arr, String sep) {
        infoFatalLogger.log(arr, sep);
    }

    public static <T> void info(T[][] arr) {
        infoFatalLogger.log(arr);
    }

    public static void info(int[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.log(arr, it, sep);
    }

    public static void info(int[] arr, Iterator<Integer> it) {
        infoFatalLogger.log(arr, it);
    }

    public static void info(long[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.log(arr, it, sep);
    }

    public static void info(long[] arr, Iterator<Integer> it) {
        infoFatalLogger.log(arr, it);
    }

    public static void info(double[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.log(arr, it, sep);
    }

    public static void info(double[] arr, Iterator<Integer> it) {
        infoFatalLogger.log(arr, it);
    }

    public static void info(char[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.log(arr, it, sep);
    }

    public static void info(char[] arr, Iterator<Integer> it) {
        infoFatalLogger.log(arr, it);
    }

    public static void info(boolean[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.log(arr, it, sep);
    }

    public static void info(boolean[] arr, Iterator<Integer> it) {
        infoFatalLogger.log(arr, it);
    }

    public static <T> void info(T[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.log(arr, it, sep);
    }

    public static <T> void info(T[] arr, Iterator<Integer> it) {
        infoFatalLogger.log(arr, it);
    }

    public static void info(String logPattern, JSONObject argsMap, int modeIdx) {
        infoFatalLogger.log(logPattern, argsMap, modeIdx);
    }

    public static void info(String logPattern, JSONObject argsMap) {
        infoFatalLogger.log(logPattern, argsMap);
    }

    public static void info(String logPattern, Object[] args, int modeIdx) {
        infoFatalLogger.log(logPattern, args, modeIdx);
    }

    public static void info(String logPattern, Object... args) {
        infoFatalLogger.log(logPattern, args);
    }

    public static void infoWithIdx(String logPattern, Object[] args, int modeIdx) {
        infoFatalLogger.logWithIdx(logPattern, args, modeIdx);
    }

    public static void infoWithIdx(String logPattern, Object... args) {
        infoFatalLogger.logWithIdx(logPattern, args);
    }

    public static void info(int row, int col) {
        infoFatalLogger.log(row, col);
    }

    public static void info(long row, long col) {
        infoFatalLogger.log(row, col);
    }

    public static void info(double row, double col) {
        infoFatalLogger.log(row, col);
    }

    public static void info(char row, char col) {
        infoFatalLogger.log(row, col);
    }

    public static void info(byte row, byte col) {
        infoFatalLogger.log(row, col);
    }

    public static void info(boolean bool01, boolean bool02) {
        infoFatalLogger.log(bool01, bool02);
    }

    public static <T1, T2> void info(T1 row, T2 col) {
        infoFatalLogger.log(row, col);
    }

    public static void infoHorizon(int n) {
        infoFatalLogger.logHorizon(n);
    }

    public static void infoHorizon() {
        infoFatalLogger.logHorizon();
    }

    public static void infoEnter() {
        infoFatalLogger.logEnter();
    }

    public static void infoEnter(int n) {
        infoFatalLogger.logEnter(n);
    }

    // ----------------------------- seps ----------------------------------------

    // fatal相关
    public static void fatal(boolean appendCRLF) {
        infoFatalLogger.err(appendCRLF);
    }

    public static void fatal() {
        infoFatalLogger.err();
    }

    public static void fatal(String str, boolean appendCRLF) {
        infoFatalLogger.err(str, appendCRLF);
    }

    public static void fatal(String obj) {
        infoFatalLogger.err(obj);
    }

    public static void fatal(Object obj, boolean appendCRLF) {
        infoFatalLogger.err(obj, appendCRLF);
    }

    public static void fatal(Object obj) {
        infoFatalLogger.err(obj);
    }

    public static void fatalf(String pattern, Object[] args, boolean appendCRLF) {
        infoFatalLogger.errf(pattern, args, appendCRLF);
    }

    public static void fatalf(String pattern, Object... args) {
        infoFatalLogger.errf(pattern, args);
    }

    public static String fatalLogPatternFormat(String content, boolean appendCRLF) {
        return infoFatalLogger.errLogPatternFormat(content, appendCRLF);
    }

    public static String fatalLogPatternFormat(String content) {
        return infoFatalLogger.errLogPatternFormat(content);
    }

    public static <T> void fatal(Iterator<T> it, String sep, boolean appendCRLF) {
        infoFatalLogger.err(it, sep, appendCRLF);
    }

    public static <T> void fatal(Iterator<T> it, boolean appendCRLF) {
        infoFatalLogger.err(it, appendCRLF);
    }

    public static <T> void fatal(Iterator<T> it) {
        infoFatalLogger.err(it);
    }

    public static <T> void fatal(Iterator<T> it, String sep) {
        infoFatalLogger.err(it, sep);
    }

    public static <T> void fatal(List<T> list, String sep, boolean appendCRLF) {
        infoFatalLogger.err(list, sep, appendCRLF);
    }

    public static <T> void fatal(List<T> list, boolean appendCRLF) {
        infoFatalLogger.err(list, appendCRLF);
    }

    public static <T> void fatal(List<T> list) {
        infoFatalLogger.err(list);
    }

    public static <T> void fatal(List<T> list, String sep) {
        infoFatalLogger.err(list, sep);
    }

    public static <T> void fatal(Set<T> set, String sep, boolean appendCRLF) {
        infoFatalLogger.err(set, sep, appendCRLF);
    }

    public static <T> void fatal(Set<T> set, boolean appendCRLF) {
        infoFatalLogger.err(set, appendCRLF);
    }

    public static <T> void fatal(Set<T> set) {
        infoFatalLogger.err(set);
    }

    public static <T> void fatal(Set<T> set, String sep) {
        infoFatalLogger.err(set, sep);
    }

    public static <K, V> void fatal(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        infoFatalLogger.err(map, kvSep, sep, appendCRLF);
    }

    public static <K, V> void fatal(Map<K, V> map, String kvSep, String sep) {
        infoFatalLogger.err(map, kvSep, sep);
    }

    public static <K, V> void fatal(Map<K, V> map, String sep, boolean appendCRLF) {
        infoFatalLogger.err(map, sep, appendCRLF);
    }

    public static <K, V> void fatal(Map<K, V> map, boolean appendCRLF) {
        infoFatalLogger.err(map, appendCRLF);
    }

    public static <K, V> void fatal(Map<K, V> map) {
        infoFatalLogger.err(map);
    }

    public static <K, V> void fatal(Map<K, V> map, String sep) {
        infoFatalLogger.err(map, sep);
    }

    public static void fatal(int[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.err(arr, sep, appendCRLF);
    }

    public static void fatal(int[] arr, boolean appendCRLF) {
        infoFatalLogger.err(arr, appendCRLF);
    }

    public static void fatal(int[] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(int[] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(long[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.err(arr, sep, appendCRLF);
    }

    public static void fatal(long[] arr, boolean appendCRLF) {
        infoFatalLogger.err(arr, appendCRLF);
    }

    public static void fatal(long[] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(long[] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(double[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.err(arr, sep, appendCRLF);
    }

    public static void fatal(double[] arr, boolean appendCRLF) {
        infoFatalLogger.err(arr, appendCRLF);
    }

    public static void fatal(double[] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(double[] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(byte[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.err(arr, sep, appendCRLF);
    }

    public static void fatal(byte[] arr, boolean appendCRLF) {
        infoFatalLogger.err(arr, appendCRLF);
    }

    public static void fatal(byte[] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(byte[] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(char[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.err(arr, sep, appendCRLF);
    }

    public static void fatal(char[] arr, boolean appendCRLF) {
        infoFatalLogger.err(arr, appendCRLF);
    }

    public static void fatal(char[] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(char[] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(boolean[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.err(arr, sep, appendCRLF);
    }

    public static void fatal(boolean[] arr, boolean appendCRLF) {
        infoFatalLogger.err(arr, appendCRLF);
    }

    public static void fatal(boolean[] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(boolean[] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static <T> void fatal(T[] arr, String sep, boolean appendCRLF) {
        infoFatalLogger.err(arr, sep, appendCRLF);
    }

    public static <T> void fatal(T[] arr, boolean appendCRLF) {
        infoFatalLogger.err(arr, appendCRLF);
    }

    public static <T> void fatal(T[] arr) {
        infoFatalLogger.err(arr);
    }

    public static <T> void fatal(T[] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(int[][] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(int[][] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(long[][] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(long[][] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(double[][] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(double[][] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(byte[][] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(byte[][] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(char[][] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(char[][] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(boolean[][] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static void fatal(boolean[][] arr) {
        infoFatalLogger.err(arr);
    }

    public static <T> void fatal(T[][] arr, String sep) {
        infoFatalLogger.err(arr, sep);
    }

    public static <T> void fatal(T[][] arr) {
        infoFatalLogger.err(arr);
    }

    public static void fatal(int[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.err(arr, it, sep);
    }

    public static void fatal(int[] arr, Iterator<Integer> it) {
        infoFatalLogger.err(arr, it);
    }

    public static void fatal(long[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.err(arr, it, sep);
    }

    public static void fatal(long[] arr, Iterator<Integer> it) {
        infoFatalLogger.err(arr, it);
    }

    public static void fatal(double[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.err(arr, it, sep);
    }

    public static void fatal(double[] arr, Iterator<Integer> it) {
        infoFatalLogger.err(arr, it);
    }

    public static void fatal(char[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.err(arr, it, sep);
    }

    public static void fatal(char[] arr, Iterator<Integer> it) {
        infoFatalLogger.err(arr, it);
    }

    public static void fatal(boolean[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.err(arr, it, sep);
    }

    public static void fatal(boolean[] arr, Iterator<Integer> it) {
        infoFatalLogger.err(arr, it);
    }

    public static <T> void fatal(T[] arr, Iterator<Integer> it, String sep) {
        infoFatalLogger.err(arr, it, sep);
    }

    public static <T> void fatal(T[] arr, Iterator<Integer> it) {
        infoFatalLogger.err(arr, it);
    }

    public static void fatal(String logPattern, JSONObject argsMap) {
        infoFatalLogger.err(logPattern, argsMap);
    }

    public static void fatal(String logPattern, Object... args) {
        infoFatalLogger.err(logPattern, args);
    }

    public static void fatalWithIdx(String logPattern, Object... args) {
        infoFatalLogger.errWithIdx(logPattern, args);
    }

    public static void fatal(int row, int col) {
        infoFatalLogger.err(row, col);
    }

    public static void fatal(long row, long col) {
        infoFatalLogger.err(row, col);
    }

    public static void fatal(double row, double col) {
        infoFatalLogger.err(row, col);
    }

    public static void fatal(char row, char col) {
        infoFatalLogger.err(row, col);
    }

    public static void fatal(byte row, byte col) {
        infoFatalLogger.err(row, col);
    }

    public static void fatal(boolean bool01, boolean bool02) {
        infoFatalLogger.err(bool01, bool02);
    }

    public static <T1, T2> void fatal(T1 row, T2 col) {
        infoFatalLogger.err(row, col);
    }

    public static void fatalHorizon(int n) {
        infoFatalLogger.errHorizon(n);
    }

    public static void fatalHorizon() {
        infoFatalLogger.errHorizon();
    }

    public static void fatalEnter() {
        infoFatalLogger.errEnter();
    }

    public static void fatalEnter(int n) {
        infoFatalLogger.errEnter(n);
    }

    // ----------------------------- seps ----------------------------------------

    // debug相关
    public static void debug(boolean appendCRLF) {
        debugWarnLogger.log(appendCRLF);
    }

    public static void debug() {
        debugWarnLogger.log();
    }

    public static void debug(String str, boolean appendCRLF) {
        debugWarnLogger.log(str, appendCRLF);
    }

    public static void debug(String obj) {
        debugWarnLogger.log(obj);
    }

    public static void debug(Object obj, boolean appendCRLF) {
        debugWarnLogger.log(obj, appendCRLF);
    }

    public static void debug(Object obj) {
        debugWarnLogger.log(obj);
    }

    public static void debugf(String pattern, Object[] args, boolean appendCRLF) {
        debugWarnLogger.logf(pattern, args, appendCRLF);
    }

    public static void debugf(String pattern, Object... args) {
        debugWarnLogger.logf(pattern, args);
    }

    public String debugLogPatternFormat(String content, boolean appendCRLF, boolean isFormat, int modeIdx) {
        return debugWarnLogger.logLogPatternFormat(content, appendCRLF, isFormat, modeIdx);
    }

    public static String debugLogPatternFormat(String content, boolean appendCRLF) {
        return debugWarnLogger.logLogPatternFormat(content, appendCRLF);
    }

    public static String debugLogPatternFormat(String content) {
        return debugWarnLogger.logLogPatternFormat(content);
    }

    public static <T> void debug(Iterator<T> it, String sep, boolean appendCRLF) {
        debugWarnLogger.log(it, sep, appendCRLF);
    }

    public static <T> void debug(Iterator<T> it, boolean appendCRLF) {
        debugWarnLogger.log(it, appendCRLF);
    }

    public static <T> void debug(Iterator<T> it) {
        debugWarnLogger.log(it);
    }

    public static <T> void debug(Iterator<T> it, String sep) {
        debugWarnLogger.log(it, sep);
    }

    public static <T> void debug(List<T> list, String sep, boolean appendCRLF) {
        debugWarnLogger.log(list, sep, appendCRLF);
    }

    public static <T> void debug(List<T> list, boolean appendCRLF) {
        debugWarnLogger.log(list, appendCRLF);
    }

    public static <T> void debug(List<T> list) {
        debugWarnLogger.log(list);
    }

    public static <T> void debug(List<T> list, String sep) {
        debugWarnLogger.log(list, sep);
    }

    public static <T> void debug(Set<T> set, String sep, boolean appendCRLF) {
        debugWarnLogger.log(set, sep, appendCRLF);
    }

    public static <T> void debug(Set<T> set, boolean appendCRLF) {
        debugWarnLogger.log(set, appendCRLF);
    }

    public static <T> void debug(Set<T> set) {
        debugWarnLogger.log(set);
    }

    public static <T> void debug(Set<T> set, String sep) {
        debugWarnLogger.log(set, sep);
    }

    public static <K, V> void debug(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        debugWarnLogger.log(map, kvSep, sep, appendCRLF);
    }

    public static <K, V> void debug(Map<K, V> map, String kvSep, String sep) {
        debugWarnLogger.log(map, kvSep, sep);
    }

    public static <K, V> void debug(Map<K, V> map, String sep, boolean appendCRLF) {
        debugWarnLogger.log(map, sep, appendCRLF);
    }

    public static <K, V> void debug(Map<K, V> map, boolean appendCRLF) {
        debugWarnLogger.log(map, appendCRLF);
    }

    public static <K, V> void debug(Map<K, V> map) {
        debugWarnLogger.log(map);
    }

    public static <K, V> void debug(Map<K, V> map, String sep) {
        debugWarnLogger.log(map, sep);
    }

    public static void debug(int[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.log(arr, sep, appendCRLF);
    }

    public static void debug(int[] arr, boolean appendCRLF) {
        debugWarnLogger.log(arr, appendCRLF);
    }

    public static void debug(int[] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(int[] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(long[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.log(arr, sep, appendCRLF);
    }

    public static void debug(long[] arr, boolean appendCRLF) {
        debugWarnLogger.log(arr, appendCRLF);
    }

    public static void debug(long[] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(long[] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(double[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.log(arr, sep, appendCRLF);
    }

    public static void debug(double[] arr, boolean appendCRLF) {
        debugWarnLogger.log(arr, appendCRLF);
    }

    public static void debug(double[] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(double[] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(byte[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.log(arr, sep, appendCRLF);
    }

    public static void debug(byte[] arr, boolean appendCRLF) {
        debugWarnLogger.log(arr, appendCRLF);
    }

    public static void debug(byte[] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(byte[] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(char[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.log(arr, sep, appendCRLF);
    }

    public static void debug(char[] arr, boolean appendCRLF) {
        debugWarnLogger.log(arr, appendCRLF);
    }

    public static void debug(char[] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(char[] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(boolean[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.log(arr, sep, appendCRLF);
    }

    public static void debug(boolean[] arr, boolean appendCRLF) {
        debugWarnLogger.log(arr, appendCRLF);
    }

    public static void debug(boolean[] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(boolean[] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static <T> void debug(T[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.log(arr, sep, appendCRLF);
    }

    public static <T> void debug(T[] arr, boolean appendCRLF) {
        debugWarnLogger.log(arr, appendCRLF);
    }

    public static <T> void debug(T[] arr) {
        debugWarnLogger.log(arr);
    }

    public static <T> void debug(T[] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(int[][] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(int[][] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(long[][] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(long[][] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(double[][] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(double[][] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(byte[][] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(byte[][] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(char[][] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(char[][] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(boolean[][] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static void debug(boolean[][] arr) {
        debugWarnLogger.log(arr);
    }

    public static <T> void debug(T[][] arr, String sep) {
        debugWarnLogger.log(arr, sep);
    }

    public static <T> void debug(T[][] arr) {
        debugWarnLogger.log(arr);
    }

    public static void debug(int[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.log(arr, it, sep);
    }

    public static void debug(int[] arr, Iterator<Integer> it) {
        debugWarnLogger.log(arr, it);
    }

    public static void debug(long[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.log(arr, it, sep);
    }

    public static void debug(long[] arr, Iterator<Integer> it) {
        debugWarnLogger.log(arr, it);
    }

    public static void debug(double[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.log(arr, it, sep);
    }

    public static void debug(double[] arr, Iterator<Integer> it) {
        debugWarnLogger.log(arr, it);
    }

    public static void debug(char[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.log(arr, it, sep);
    }

    public static void debug(char[] arr, Iterator<Integer> it) {
        debugWarnLogger.log(arr, it);
    }

    public static void debug(boolean[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.log(arr, it, sep);
    }

    public static void debug(boolean[] arr, Iterator<Integer> it) {
        debugWarnLogger.log(arr, it);
    }

    public static <T> void debug(T[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.log(arr, it, sep);
    }

    public static void debug(String logPattern, JSONObject argsMap, int modeIdx) {
        debugWarnLogger.log(logPattern, argsMap, modeIdx);
    }

    public static void debug(String logPattern, JSONObject argsMap) {
        debugWarnLogger.log(logPattern, argsMap);
    }

    public static void debug(String logPattern, Object[] args, int modeIdx) {
        debugWarnLogger.log(logPattern, args, modeIdx);
    }

    public static void debug(String logPattern, Object... args) {
        debugWarnLogger.log(logPattern, args);
    }

    public static void debugWithIdx(String logPattern, Object[] args, int modeIdx) {
        debugWarnLogger.logWithIdx(logPattern, args, modeIdx);
    }

    public static void debugWithIdx(String logPattern, Object... args) {
        debugWarnLogger.logWithIdx(logPattern, args);
    }

    public static <T> void debug(T[] arr, Iterator<Integer> it) {
        debugWarnLogger.log(arr, it);
    }

    public static void debug(int row, int col) {
        debugWarnLogger.log(row, col);
    }

    public static void debug(long row, long col) {
        debugWarnLogger.log(row, col);
    }

    public static void debug(double row, double col) {
        debugWarnLogger.log(row, col);
    }

    public static void debug(char row, char col) {
        debugWarnLogger.log(row, col);
    }

    public static void debug(byte row, byte col) {
        debugWarnLogger.log(row, col);
    }

    public static void debug(boolean bool01, boolean bool02) {
        debugWarnLogger.log(bool01, bool02);
    }

    public static <T1, T2> void debug(T1 row, T2 col) {
        debugWarnLogger.log(row, col);
    }

    public static void debugHorizon(int n) {
        debugWarnLogger.logHorizon(n);
    }

    public static void debugHorizon() {
        debugWarnLogger.logHorizon();
    }

    public static void debugEnter() {
        debugWarnLogger.logEnter();
    }

    public static void debugEnter(int n) {
        debugWarnLogger.logEnter(n);
    }

    // ----------------------------- seps ----------------------------------------

    // warn相关
    public static void warn(boolean appendCRLF) {
        debugWarnLogger.err(appendCRLF);
    }

    public static void warn() {
        debugWarnLogger.err();
    }

    public static void warn(String str, boolean appendCRLF) {
        debugWarnLogger.err(str, appendCRLF);
    }

    public static void warn(String obj) {
        debugWarnLogger.err(obj);
    }

    public static void warn(Object obj, boolean appendCRLF) {
        debugWarnLogger.err(obj, appendCRLF);
    }

    public static void warn(Object obj) {
        debugWarnLogger.err(obj);
    }

    public static void warnf(String pattern, Object[] args, boolean appendCRLF) {
        debugWarnLogger.errf(pattern, args, appendCRLF);
    }

    public static void warnf(String pattern, Object... args) {
        debugWarnLogger.errf(pattern, args);
    }

    public static String warnLogPatternFormat(String content, boolean appendCRLF) {
        return debugWarnLogger.errLogPatternFormat(content, appendCRLF);
    }

    public static String warnLogPatternFormat(String content) {
        return debugWarnLogger.errLogPatternFormat(content);
    }

    public static <T> void warn(Iterator<T> it, String sep, boolean appendCRLF) {
        debugWarnLogger.err(it, sep, appendCRLF);
    }

    public static <T> void warn(Iterator<T> it, boolean appendCRLF) {
        debugWarnLogger.err(it, appendCRLF);
    }

    public static <T> void warn(Iterator<T> it) {
        debugWarnLogger.err(it);
    }

    public static <T> void warn(Iterator<T> it, String sep) {
        debugWarnLogger.err(it, sep);
    }

    public static <T> void warn(List<T> list, String sep, boolean appendCRLF) {
        debugWarnLogger.err(list, sep, appendCRLF);
    }

    public static <T> void warn(List<T> list, boolean appendCRLF) {
        debugWarnLogger.err(list, appendCRLF);
    }

    public static <T> void warn(List<T> list) {
        debugWarnLogger.err(list);
    }

    public static <T> void warn(List<T> list, String sep) {
        debugWarnLogger.err(list, sep);
    }

    public static <T> void warn(Set<T> set, String sep, boolean appendCRLF) {
        debugWarnLogger.err(set, sep, appendCRLF);
    }

    public static <T> void warn(Set<T> set, boolean appendCRLF) {
        debugWarnLogger.err(set, appendCRLF);
    }

    public static <T> void warn(Set<T> set) {
        debugWarnLogger.err(set);
    }

    public static <T> void warn(Set<T> set, String sep) {
        debugWarnLogger.err(set, sep);
    }

    public static <K, V> void warn(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        debugWarnLogger.err(map, kvSep, sep, appendCRLF);
    }

    public static <K, V> void warn(Map<K, V> map, String kvSep, String sep) {
        debugWarnLogger.err(map, kvSep, sep);
    }

    public static <K, V> void warn(Map<K, V> map, String sep, boolean appendCRLF) {
        debugWarnLogger.err(map, sep, appendCRLF);
    }

    public static <K, V> void warn(Map<K, V> map, boolean appendCRLF) {
        debugWarnLogger.err(map, appendCRLF);
    }

    public static <K, V> void warn(Map<K, V> map) {
        debugWarnLogger.err(map);
    }

    public static <K, V> void warn(Map<K, V> map, String sep) {
        debugWarnLogger.err(map, sep);
    }

    public static void warn(int[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.err(arr, sep, appendCRLF);
    }

    public static void warn(int[] arr, boolean appendCRLF) {
        debugWarnLogger.err(arr, appendCRLF);
    }

    public static void warn(int[] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(int[] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(long[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.err(arr, sep, appendCRLF);
    }

    public static void warn(long[] arr, boolean appendCRLF) {
        debugWarnLogger.err(arr, appendCRLF);
    }

    public static void warn(long[] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(long[] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(double[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.err(arr, sep, appendCRLF);
    }

    public static void warn(double[] arr, boolean appendCRLF) {
        debugWarnLogger.err(arr, appendCRLF);
    }

    public static void warn(double[] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(double[] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(byte[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.err(arr, sep, appendCRLF);
    }

    public static void warn(byte[] arr, boolean appendCRLF) {
        debugWarnLogger.err(arr, appendCRLF);
    }

    public static void warn(byte[] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(byte[] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(char[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.err(arr, sep, appendCRLF);
    }

    public static void warn(char[] arr, boolean appendCRLF) {
        debugWarnLogger.err(arr, appendCRLF);
    }

    public static void warn(char[] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(char[] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(boolean[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.err(arr, sep, appendCRLF);
    }

    public static void warn(boolean[] arr, boolean appendCRLF) {
        debugWarnLogger.err(arr, appendCRLF);
    }

    public static void warn(boolean[] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(boolean[] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static <T> void warn(T[] arr, String sep, boolean appendCRLF) {
        debugWarnLogger.err(arr, sep, appendCRLF);
    }

    public static <T> void warn(T[] arr, boolean appendCRLF) {
        debugWarnLogger.err(arr, appendCRLF);
    }

    public static <T> void warn(T[] arr) {
        debugWarnLogger.err(arr);
    }

    public static <T> void warn(T[] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(int[][] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(int[][] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(long[][] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(long[][] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(double[][] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(double[][] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(byte[][] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(byte[][] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(char[][] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(char[][] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(boolean[][] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static void warn(boolean[][] arr) {
        debugWarnLogger.err(arr);
    }

    public static <T> void warn(T[][] arr, String sep) {
        debugWarnLogger.err(arr, sep);
    }

    public static <T> void warn(T[][] arr) {
        debugWarnLogger.err(arr);
    }

    public static void warn(int[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.err(arr, it, sep);
    }

    public static void warn(int[] arr, Iterator<Integer> it) {
        debugWarnLogger.err(arr, it);
    }

    public static void warn(long[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.err(arr, it, sep);
    }

    public static void warn(long[] arr, Iterator<Integer> it) {
        debugWarnLogger.err(arr, it);
    }

    public static void warn(double[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.err(arr, it, sep);
    }

    public static void warn(double[] arr, Iterator<Integer> it) {
        debugWarnLogger.err(arr, it);
    }

    public static void warn(char[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.err(arr, it, sep);
    }

    public static void warn(char[] arr, Iterator<Integer> it) {
        debugWarnLogger.err(arr, it);
    }

    public static void warn(boolean[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.err(arr, it, sep);
    }

    public static void warn(boolean[] arr, Iterator<Integer> it) {
        debugWarnLogger.err(arr, it);
    }

    public static <T> void warn(T[] arr, Iterator<Integer> it, String sep) {
        debugWarnLogger.err(arr, it, sep);
    }

    public static <T> void warn(T[] arr, Iterator<Integer> it) {
        debugWarnLogger.err(arr, it);
    }

    public static void warn(String logPattern, JSONObject argsMap) {
        debugWarnLogger.err(logPattern, argsMap);
    }

    public static void warn(String logPattern, Object... args) {
        debugWarnLogger.err(logPattern, args);
    }

    public static void warnWithIdx(String logPattern, Object... args) {
        debugWarnLogger.errWithIdx(logPattern, args);
    }

    public static void warn(int row, int col) {
        debugWarnLogger.err(row, col);
    }

    public static void warn(long row, long col) {
        debugWarnLogger.err(row, col);
    }

    public static void warn(double row, double col) {
        debugWarnLogger.err(row, col);
    }

    public static void warn(char row, char col) {
        debugWarnLogger.err(row, col);
    }

    public static void warn(byte row, byte col) {
        debugWarnLogger.err(row, col);
    }

    public static void warn(boolean bool01, boolean bool02) {
        debugWarnLogger.err(bool01, bool02);
    }

    public static <T1, T2> void warn(T1 row, T2 col) {
        debugWarnLogger.err(row, col);
    }

    public static void warnHorizon(int n) {
        debugWarnLogger.errHorizon(n);
    }

    public static void warnHorizon() {
        debugWarnLogger.errHorizon();
    }

    public static void warnEnter() {
        debugWarnLogger.errEnter();
    }

    public static void warnEnter(int n) {
        debugWarnLogger.errEnter(n);
    }

    // 刷出缓冲区的数据		add at 2016.04.15
    public static void flush() {
        log.flush();
        infoFatalLogger.flush();
        debugWarnLogger.flush();
    }

    // ------------ 待续 --------------------


}
