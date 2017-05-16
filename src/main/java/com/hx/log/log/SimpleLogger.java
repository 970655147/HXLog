/**
 * file name : SimpleLogger.java
 * created at : 9:34:52 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.log;

import com.hx.json.JSONObject;
import com.hx.log.idx.IdxGenerator;
import com.hx.log.io.BuffInfo;
import com.hx.log.log.interf.Logger;
import com.hx.log.log.log_pattern.LogPatternChain;
import com.hx.log.util.Constants;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.io.OutputStream;
import java.util.*;
import java.util.Map.Entry;

/**
 * 日志工具类
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 5:13 PM
 */
public class SimpleLogger implements Logger {

    /**
     * 生成Log索引的工具
     */
    private static IdxGenerator IDX_GENERATOR = new IdxGenerator();
    /**
     * 索引名称的前缀
     */
    public static final String BUFF_NAME_PREFIX = Constants.optString(LoggerConstants._BUFF_NAME_PREFIX);
    /**
     * 索引名称之间的分隔符
     */
    public static final String BUFF_NAME_SEP = Constants.optString(LoggerConstants._BUFF_NAME_SEP);

    // --------------------------- 可配置变量 --------------------------------------
    // 以及输出流, 错误流, 以及默认是否换行
    /**
     * 水平线
     */
    public String horizonLines = Constants.optString(LoggerConstants._HORIZON_LINES);
    /**
     * 水平星
     */
    public String horizonStars = Constants.optString(LoggerConstants._HORIZON_STARS);
    /**
     * got there
     */
    public String gotThere = Constants.optString(LoggerConstants._GOT_THERE);
    /**
     * got nothing
     */
    public String gotNothing = Constants.optString(LoggerConstants._GOT_NOTHING);

    /**
     * 当前logger的id
     */
    public final int loggerId = IDX_GENERATOR.next();
    /**
     * loggerId被logHandler处理之后的结果
     */
    public final String loggerIdx = Constants.LOG_IDX_HANDLER_PARSER.handle(String.valueOf(loggerId));
    /**
     * out, err 输出流
     */
    public OutputStream[] outStreams = Arrays.copyOf(LoggerConstants.OUT_STREAMS, LoggerConstants.OUT_STREAMS.length);
    /**
     * out, err 是否输出到文件
     */
    private final boolean[] outToLogFile = Arrays.copyOf(LoggerConstants.OUT_TO_LOG_FILES, LoggerConstants.OUT_TO_LOG_FILES.length);
    /**
     * out, err buffNames
     */
    private final String[] logBuffNames = new String[LoggerConstants.LOG_BUFF_SIFFIXES.length];

    /**
     * 初始化logBuffNames
     */ {
        for (int i = 0; i < logBuffNames.length; i++) {
            logBuffNames[i] = genLogBuffNames(LoggerConstants.LOG_BUFF_SIFFIXES[i]);
        }
    }

    /**
     * out, err 输出文件
     */
    private String[] logFiles = Arrays.copyOf(LoggerConstants.LOG_FILES, LoggerConstants.LOG_FILES.length);
    /**
     * out, err 输出模式的名称
     */
    private String[] logModes = Arrays.copyOf(Constants.LOG_MODES, Constants.LOG_MODES.length);
    /**
     * out, err logPatternChain
     */
    public LogPatternChain logPatternChain = Constants.LOG_PATTERN.copyOf();

    /**
     * 有crlf输出各个元素的分隔符
     */
    public String defaultSepWhileCRLF = Constants.optString(LoggerConstants._DEFAULT_SEP_WHILE_CRLF);
    /**
     * 无crlf输出各个元素的分隔符
     */
    public String defaultSepWhileNotCrlf = Constants.optString(LoggerConstants._DEFAULT_SEP_WHILE_NOT_CRLF);
    /**
     * 二维数组的各个元素的分隔符
     */
    public String defaultSepWhileTwoDimen = Constants.optString(LoggerConstants._DEFAULT_SEP_WHILE_TWO_DIMEN);
    /**
     * map元素的kv分隔符
     */
    public String defaultSepWhileMapKV = Constants.optString(LoggerConstants._DEFAULT_SEP_MAP_KVSEP);

    /**
     * out 输出 默认是否添加crlf
     */
    public boolean outputAppendCrlf = Constants.optBoolean(LoggerConstants._DEFAULT_OUTPUT_APPEND_CRLF);
    /**
     * err 输出 默认是否添加crlf
     */
    public boolean errputAppendCrlf = Constants.optBoolean(LoggerConstants._DEFAULT_ERRPUT_APPEND_CRLF);
    /**
     * out 集合输出 默认是否添加crlf
     */
    public boolean outputAppendCrlfForContainer = Constants.optBoolean(LoggerConstants._DEFAULT_OUTPUT_APPEND_CRLF_FOR_CONTAINER);
    /**
     * err 集合输出 默认是否添加crlf
     */
    public boolean errputAppendCrlfForContainer = Constants.optBoolean(LoggerConstants._DEFAULT_ERRPUT_APPEND_CRLF_FOR_CONTAINER);
    /**
     * out 格式化输出 默认是否添加crlf
     */
    public boolean outputAppendCrlfForFormat = Constants.optBoolean(LoggerConstants._DEFAULT_OUTPUT_APPEND_CRLF_FOR_FORMAT);
    /**
     * err 格式化输出 默认是否添加crlf
     */
    public boolean errputAppendCrlfForFormat = Constants.optBoolean(LoggerConstants._DEFAULT_ERRPUT_APPEND_CRLF_FOR_FORMAT);
    /**
     * 输出 是否使用logPattern格式化
     */
    public boolean isFormat = Constants.optBoolean(LoggerConstants._DEFAULT_IS_FORMAT);

    // --------------------------- 置于最后 ----------------------------------------

    /**
     *  初始化logFiles
     */ {
        try {
            for (int i = 0; i < logModes.length; i++) {
                if (outToLogFile[i]) {
                    setLogFile0(logFiles[i], i);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // --------------------------- 配置可配置变量的接口 ----------------------------------------

    @Override
    public void setOutLogFile(String logFile) throws Exception {
        setLogFile0(logFile, Constants.OUT_IDX);
    }

    @Override
    public void setErrLogFile(String logFile) throws Exception {
        setLogFile0(logFile, Constants.ERR_IDX);
    }

    @Override
    public void setOutMode(String mode) {
        logModes[Constants.OUT_IDX] = mode;
    }

    @Override
    public void setErrMode(String mode) {
        logModes[Constants.ERR_IDX] = mode;
    }

    @Override
    public String getMode(int modeIdx) {
        return logModes[modeIdx];
    }

    @Override
    public String getOutMode() {
        return logModes[Constants.OUT_IDX];
    }

    @Override
    public String getErrMode() {
        return logModes[Constants.ERR_IDX];
    }

    @Override
    public void setOutStream(OutputStream stream) {
        outStreams[Constants.OUT_IDX] = stream;
    }

    @Override
    public void setErrStream(OutputStream stream) {
        outStreams[Constants.ERR_IDX] = stream;
    }

    @Override
    public void setOutToLogFile(boolean outToLogFile, String logFile) throws Exception {
        setToLogFile0(outToLogFile, logFile, Constants.OUT_IDX);
    }

    @Override
    public void setOutToLogFile(boolean outToLogFile) throws Exception {
        setOutToLogFile(outToLogFile, logFiles[Constants.OUT_IDX]);
    }

    @Override
    public void setErrToLogFile(boolean errToLogFile, String logFile) throws Exception {
        setToLogFile0(errToLogFile, logFile, Constants.ERR_IDX);
    }

    @Override
    public void setErrToLogFile(boolean errToLogFile) throws Exception {
        setErrToLogFile(errToLogFile, logFiles[Constants.ERR_IDX]);
    }

    @Override
    public void dispathLogInfo(int modeIdx, String logStr, boolean isFormat) {
        // dispatch
        switch (modeIdx) {
            case Constants.OUT_IDX:
                log(logStr, outputAppendCrlf, isFormat, modeIdx);
                break;
            case Constants.ERR_IDX:
                log(logStr, errputAppendCrlf, isFormat, modeIdx);
                break;
            default:
                err("have no this 'modeIdx', currentStartIdx support " + Constants.LOG_MODES_STR + " ");
                break;
        }
    }

    @Override
    public void dispathLogInfo(int modeIdx, String logStr) {
        dispathLogInfo(modeIdx, logStr, isFormat);
    }

    // --------------------------- 业务方法 ----------------------------------------

    @Override
    public void log(String str, boolean appendCRLF, boolean isFormat, int modeIdx) {
        if (LogLevel.of(this.getMode(modeIdx)).gte(Log.LOG_LEVEL_MIN)) {
//		Tools.assert0(str != null, "'str' is null ");
            str = String.valueOf(str);

            // switch of 'isFormat'
            String line = logLogPatternFormat(str, appendCRLF, isFormat, modeIdx);

            // add 'outStreams != null' for rubustness		add at 2016.05.30
            if ((outStreams != null) && ((modeIdx < 0) || (modeIdx >= outStreams.length))) {
                err("have no this 'modeIdx', currentStartIdx support " + Constants.LOG_MODES_STR + " ");
                return;
            }

            try {
                if ((outStreams != null) && (outStreams[modeIdx] != null)) {
                    outStreams[modeIdx].write(line.getBytes(Tools.DEFAULT_CHARSET));
                }
                if (outToLogFile[modeIdx]) {
                    Tools.appendBuffer(logBuffNames[modeIdx], line);
                }
            } catch (Exception e) {
                Tools.assert0(Tools.errorMsg(e));
            }
        }
    }

    @Override
    public void log(boolean value) {
        log(String.valueOf(value), outputAppendCrlf);
    }

    @Override
    public void log() {
        log(gotThere, outputAppendCrlf);
    }

    @Override
    public void log(String str, boolean appendCRLF, int modeIdx) {
        log(str, appendCRLF, true, modeIdx);
    }

    @Override
    public void log(String str, boolean appendCRLF) {
        log(str, appendCRLF, Constants.OUT_IDX);
    }

    @Override
    public void log(String obj) {
        log(obj, outputAppendCrlf);
    }

    @Override
    public void log(Object obj, boolean appendCRLF) {
        log(String.valueOf(obj), appendCRLF);
    }

    @Override
    public void log(Object obj) {
        log(obj, outputAppendCrlf);
    }

    @Override
    public void logf(String pattern, Object[] args, boolean appendCRLF) {
        log(String.format(pattern, args), appendCRLF);
    }

    @Override
    public void logf(String pattern, Object... args) {
        logf(pattern, args, outputAppendCrlf);
    }

    @Override
    public String logLogPatternFormat(String content, boolean appendCRLF, boolean isFormat, int modeIdx) {
        StringBuilder sb = new StringBuilder(content.length() + 4);
        if (isFormat) {
            sb.append(
                    LogPatternUtils.formatLogInfo(logPatternChain, new JSONObject().element(LogPatternConstants.LOG_PATTERN_MSG, content)
                            .element(LogPatternConstants.LOG_PATTERN_MODE, logModes[Tools.getIdx(modeIdx, logModes.length, Constants.ERR_IDX)])
                            .element(LogPatternConstants.LOG_PATTERN_LOG_IDX, loggerIdx)
                    ));
        } else {
            sb.append(content);
        }

        if (appendCRLF) {
            sb.append(Constants.CRLF);
        }
        return sb.toString();
    }

    @Override
    public String logLogPatternFormat(String content, boolean appendCRLF) {
        return logLogPatternFormat(content, appendCRLF, isFormat, Constants.OUT_IDX);
    }

    @Override
    public String logLogPatternFormat(String content) {
        return logLogPatternFormat(content, outputAppendCrlfForFormat);
    }

    @Override
    public <T> void log(Iterator<T> it, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(it != null, "'Iterator' is null ");
        Tools.assert0(sep != null, "'Seprator' is null ");

        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            while (it.hasNext()) {
                Tools.appendCRLF(sb, (it.next().toString() + sep));
            }
        } else {
            while (it.hasNext()) {
                Tools.append(sb, it.next().toString() + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        // dispatch
        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public <T> void log(Iterator<T> it, String sep, boolean appendCRLF) {
        log(it, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public <T> void log(Iterator<T> it, boolean appendCRLF) {
        if (appendCRLF) {
            log(it, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(it, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public <T> void log(Iterator<T> it) {
        log(it, outputAppendCrlfForContainer);
    }

    @Override
    public <T> void log(Iterator<T> it, String sep) {
        log(it, sep, outputAppendCrlfForContainer);
    }

    @Override
    public <T> void log(List<T> list, String sep, boolean appendCRLF) {
        Tools.assert0(list != null, "'list' is null ");
        log(list.iterator(), sep, appendCRLF);
    }

    @Override
    public <T> void log(List<T> list, boolean appendCRLF) {
        Tools.assert0(list != null, "'list' is null ");
        log(list.iterator(), appendCRLF);
    }

    @Override
    public <T> void log(List<T> list) {
        Tools.assert0(list != null, "'list' is null ");
        log(list.iterator(), outputAppendCrlfForContainer);
    }

    @Override
    public <T> void log(List<T> list, String sep) {
        Tools.assert0(list != null, "'list' is null ");
        log(list.iterator(), sep, outputAppendCrlfForContainer);
    }

    @Override
    public <T> void log(Set<T> set, String sep, boolean appendCRLF) {
        Tools.assert0(set != null, "'set' is null ");
        log(set.iterator(), sep, appendCRLF);
    }

    @Override
    public <T> void log(Set<T> set, boolean appendCRLF) {
        Tools.assert0(set != null, "'set' is null ");
        log(set.iterator(), appendCRLF);
    }

    @Override
    public <T> void log(Set<T> set) {
        Tools.assert0(set != null, "'set' is null ");
        log(set.iterator(), outputAppendCrlfForContainer);
    }

    @Override
    public <T> void log(Set<T> set, String sep) {
        Tools.assert0(set != null, "'set' is null ");
        log(set.iterator(), sep, outputAppendCrlfForContainer);
    }

    @Override
    public <K, V> void log(Map<K, V> map, String kvSep, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(map != null, "'map' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (Entry<K, V> entry : map.entrySet()) {
                Tools.appendCRLF(sb, entry.getKey() + kvSep + entry.getValue() + sep);
            }
        } else {
            for (Entry<K, V> entry : map.entrySet()) {
                Tools.append(sb, entry.getKey() + " -> " + entry.getValue() + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        // dispatch
        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public <K, V> void log(Map<K, V> map, String sep, int modeIdx, boolean appendCRLF) {
        log(map, defaultSepWhileMapKV, sep, modeIdx, appendCRLF);
    }

    @Override
    public <K, V> void log(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        log(map, kvSep, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public <K, V> void log(Map<K, V> map, String kvSep, String sep) {
        log(map, kvSep, sep, true);
    }

    @Override
    public <K, V> void log(Map<K, V> map, String sep, boolean appendCRLF) {
        log(map, defaultSepWhileMapKV, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public <K, V> void log(Map<K, V> map, boolean appendCRLF) {
        if (appendCRLF) {
            log(map, defaultSepWhileMapKV, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(map, defaultSepWhileMapKV, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public <K, V> void log(Map<K, V> map) {
        log(map, outputAppendCrlfForContainer);
    }

    @Override
    public <K, V> void log(Map<K, V> map, String sep) {
        log(map, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(boolean[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (boolean obj : arr) {
                Tools.appendCRLF(sb, (obj + sep));
            }
        } else {
            for (boolean obj : arr) {
                Tools.append(sb, obj + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(boolean[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public void log(boolean[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void log(boolean[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public void log(boolean[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(byte[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (byte obj : arr) {
                Tools.appendCRLF(sb, (obj + sep));
            }
        } else {
            for (byte obj : arr) {
                Tools.append(sb, obj + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(byte[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public void log(byte[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void log(byte[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public void log(byte[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(char[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (char obj : arr) {
                Tools.appendCRLF(sb, (obj + sep));
            }
        } else {
            for (char obj : arr) {
                Tools.append(sb, obj + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(char[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public void log(char[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void log(char[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public void log(char[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(int[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (int obj : arr) {
                Tools.appendCRLF(sb, (obj + sep));
            }
        } else {
            for (int obj : arr) {
                Tools.append(sb, obj + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(int[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public void log(int[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void log(int[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public void log(int[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(long[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (long obj : arr) {
                Tools.appendCRLF(sb, (obj + sep));
            }
        } else {
            for (long obj : arr) {
                Tools.append(sb, obj + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(long[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public void log(long[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void log(long[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public void log(long[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(float[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (float obj : arr) {
                Tools.appendCRLF(sb, (obj + sep));
            }
        } else {
            for (float obj : arr) {
                Tools.append(sb, obj + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(float[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public void log(float[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void log(float[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public void log(float[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(double[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (double obj : arr) {
                Tools.appendCRLF(sb, (obj + sep));
            }
        } else {
            for (double obj : arr) {
                Tools.append(sb, obj + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(double[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public void log(double[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void log(double[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public void log(double[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public <T> void log(T[] arr, String sep, int modeIdx, boolean appendCRLF) {
        Tools.assert0(arr != null, "'arr' is null ");
        StringBuilder sb = new StringBuilder();
        if (appendCRLF) {
            for (Object obj : arr) {
                Tools.appendCRLF(sb, (obj.toString() + sep));
            }
        } else {
            for (Object obj : arr) {
                Tools.append(sb, obj.toString() + sep);
            }
        }
        // incase of 'it.hasNext == false'
        Tools.removeLastSep(sb, sep);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public <T> void log(T[] ls, String sep, boolean appendCRLF) {
        log(ls, sep, Constants.OUT_IDX, appendCRLF);
    }

    @Override
    public <T> void log(T[] ls, boolean appendCRLF) {
        if (appendCRLF) {
            log(ls, defaultSepWhileCRLF, appendCRLF);
        } else {
            log(ls, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public <T> void log(T[] ls) {
        log(ls, outputAppendCrlfForContainer);
    }

    @Override
    public <T> void log(T[] ls, String sep) {
        log(ls, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void log(boolean[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            boolean[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public void log(boolean[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(boolean[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(byte[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            byte[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public void log(byte[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(byte[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(char[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            char[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public void log(char[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(char[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(int[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            int[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public void log(int[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(int[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(long[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            long[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public void log(long[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(long[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(float[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(float[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(float[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            float[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public void log(double[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(double[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(double[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            double[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public <T> void log(T[][] arr, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        for (int i = 0; i < arr.length; i++) {
            Object[] row = arr[i];
            log(row, sep, modeIdx, false);
        }
    }

    @Override
    public <T> void log(T[][] arr, String sep) {
        log(arr, sep, Constants.OUT_IDX);
    }

    @Override
    public <T> void log(T[][] arr) {
        log(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void log(boolean[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(boolean[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(boolean[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void log(byte[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(byte[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(byte[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void log(char[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(char[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(char[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void log(int[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(int[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(int[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void log(long[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(long[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(long[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void log(float[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(float[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(float[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void log(double[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void log(double[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public void log(double[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public <T> void log(T[] arr, Iterator<Integer> it, String sep, int modeIdx) {
        Tools.assert0(arr != null, "'arr' is null ");
        Tools.assert0(it != null, "'Iterator' is null ");

        StringBuilder sb = new StringBuilder();
        while (it.hasNext()) {
            Tools.append(sb, String.valueOf(arr[it.next()]) + sep);
        }
        Tools.removeLastSep(sb, sep);
        sb.append(Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public <T> void log(T[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.OUT_IDX);
    }

    @Override
    public <T> void log(T[] arr, Iterator<Integer> it) {
        log(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void log(String logPattern, JSONObject argsMap, int modeIdx) {
        Tools.assert0(logPattern != null, "'arr' is null ");

        String formatted = LogPatternUtils.formatLogInfo(logPattern, argsMap);
        log(formatted, outputAppendCrlf, modeIdx);
    }

    @Override
    public void log(String logPattern, JSONObject argsMap) {
        log(logPattern, argsMap, Constants.OUT_IDX);
    }

    @Override
    public void log(String logPattern, Object[] args, int modeIdx) {
        Tools.assert0(logPattern != null, "'arr' is null ");

        String formatted = LogPatternUtils.formatLogInfo(logPattern, args);
        log(formatted, outputAppendCrlf, modeIdx);
    }

    @Override
    public <T> void log(String logPattern, Object... args) {
        log(logPattern, args, Constants.OUT_IDX);
    }

    @Override
    public void logWithIdx(String logPattern, Object[] args, int modeIdx) {
        Tools.assert0(logPattern != null, "'arr' is null ");

        String formatted = LogPatternUtils.formatLogInfoWithIdx(logPattern, args);
        log(formatted, outputAppendCrlf, modeIdx);
    }

    @Override
    public <T> void logWithIdx(String logPattern, Object... args) {
        logWithIdx(logPattern, args, Constants.OUT_IDX);
    }

    @Override
    public void log(boolean bool01, boolean bool02) {
        log(String.valueOf(bool01) + defaultSepWhileNotCrlf + String.valueOf(bool02));
    }

    @Override
    public void log(byte row, byte col) {
        log(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void log(char row, char col) {
        log(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void log(int row, int col) {
        log(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void log(long row, long col) {
        log(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void log(float row, float col) {
        log(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void log(double row, double col) {
        log(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public <T1, T2> void log(T1 row, T2 col) {
        log(row.toString() + defaultSepWhileNotCrlf + col.toString());
    }

    @Override
    public void logHorizon(int n, int modeIdx) {
        StringBuilder sb = new StringBuilder(n * (horizonLines.length() + 2));
        for (int i = 0; i < n; i++) {
            Tools.appendCRLF(sb, horizonLines);
        }
        Tools.removeLastSep(sb, Tools.CRLF);

        dispathLogInfo(modeIdx, sb.toString());
    }

    @Override
    public void logHorizon(int n) {
        logHorizon(n, Constants.OUT_IDX);
    }

    @Override
    public void logHorizon() {
        logHorizon(1);
    }

    @Override
    public void logEnter(int n, int modeIdx) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) {
            sb.append(Tools.CRLF);
        }

        dispathLogInfo(modeIdx, sb.toString(), false);
    }

    @Override
    public void logEnter() {
        logEnter(1);
    }

    @Override
    public void logEnter(int n) {
        logEnter(n, Constants.OUT_IDX);
    }

    // ----------------------------- seps ----------------------------------------

    @Override
    public void err(boolean appendCRLF) {
        err(String.valueOf(appendCRLF), errputAppendCrlf);
    }

    @Override
    public void err() {
        err(gotThere, errputAppendCrlf);
    }

    @Override
    public void err(String str, boolean appendCRLF) {
        log(str, appendCRLF, Constants.ERR_IDX);
    }

    @Override
    public void err(String obj) {
        err(obj, errputAppendCrlf);
    }

    @Override
    public void err(Object obj, boolean appendCRLF) {
        err(String.valueOf(obj), appendCRLF);
    }

    @Override
    public void err(Object obj) {
        err(obj, errputAppendCrlf);
    }

    @Override
    public void errf(String pattern, Object[] args, boolean appendCRLF) {
        err(String.format(pattern, args), appendCRLF);
    }

    @Override
    public void errf(String pattern, Object... args) {
        errf(pattern, args, errputAppendCrlf);
    }

    @Override
    public String errLogPatternFormat(String content, boolean appendCRLF) {
        return logLogPatternFormat(content, appendCRLF, isFormat, Constants.ERR_IDX);
    }

    @Override
    public String errLogPatternFormat(String content) {
        return errLogPatternFormat(content, errputAppendCrlfForFormat);
    }

    @Override
    public <T> void err(Iterator<T> it, String sep, boolean appendCRLF) {
        log(it, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public <T> void err(Iterator<T> it, boolean appendCRLF) {
        if (appendCRLF) {
            err(it, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(it, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public <T> void err(Iterator<T> it) {
        err(it, errputAppendCrlfForContainer);
    }

    @Override
    public <T> void err(Iterator<T> it, String sep) {
        err(it, sep, errputAppendCrlfForContainer);
    }

    @Override
    public <T> void err(List<T> list, String sep, boolean appendCRLF) {
        Tools.assert0(list != null, "'list' is null ");
        err(list.iterator(), sep, appendCRLF);
    }

    @Override
    public <T> void err(List<T> list, boolean appendCRLF) {
        Tools.assert0(list != null, "'list' is null ");
        err(list.iterator(), appendCRLF);
    }

    @Override
    public <T> void err(List<T> list) {
        Tools.assert0(list != null, "'list' is null ");
        err(list.iterator(), errputAppendCrlfForContainer);
    }

    @Override
    public <T> void err(List<T> list, String sep) {
        Tools.assert0(list != null, "'list' is null ");
        err(list.iterator(), sep, errputAppendCrlfForContainer);
    }

    @Override
    public <T> void err(Set<T> set, String sep, boolean appendCRLF) {
        Tools.assert0(set != null, "'set' is null ");
        err(set.iterator(), sep, appendCRLF);
    }

    @Override
    public <T> void err(Set<T> set, boolean appendCRLF) {
        Tools.assert0(set != null, "'set' is null ");
        err(set.iterator(), appendCRLF);
    }

    @Override
    public <T> void err(Set<T> set) {
        Tools.assert0(set != null, "'set' is null ");
        err(set.iterator(), errputAppendCrlfForContainer);
    }

    @Override
    public <T> void err(Set<T> set, String sep) {
        Tools.assert0(set != null, "'set' is null ");
        err(set.iterator(), sep, errputAppendCrlfForContainer);
    }

    @Override
    public <K, V> void err(Map<K, V> map, String kvSep, String sep, boolean appendCRLF) {
        log(map, kvSep, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public <K, V> void err(Map<K, V> map, String kvSep, String sep) {
        err(map, kvSep, sep, true);
    }

    @Override
    public <K, V> void err(Map<K, V> map, String sep, boolean appendCRLF) {
        log(map, defaultSepWhileMapKV, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public <K, V> void err(Map<K, V> map, boolean appendCRLF) {
        if (appendCRLF) {
            err(map, defaultSepWhileMapKV, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(map, defaultSepWhileMapKV, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public <K, V> void err(Map<K, V> map) {
        err(map, outputAppendCrlfForContainer);
    }

    @Override
    public <K, V> void err(Map<K, V> map, String sep) {
        err(map, sep, outputAppendCrlfForContainer);
    }

    @Override
    public void err(boolean[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public void err(boolean[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void err(boolean[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public void err(boolean[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public void err(byte[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public void err(byte[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void err(byte[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public void err(byte[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public void err(char[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public void err(char[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void err(char[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public void err(char[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public void err(int[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public void err(int[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void err(int[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public void err(int[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public void err(long[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public void err(long[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void err(long[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public void err(long[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public void err(float[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public void err(float[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void err(float[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public void err(float[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public void err(double[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public void err(double[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public void err(double[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public void err(double[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public <T> void err(T[] arr, String sep, boolean appendCRLF) {
        log(arr, sep, Constants.ERR_IDX, appendCRLF);
    }

    @Override
    public <T> void err(T[] arr, boolean appendCRLF) {
        if (appendCRLF) {
            err(arr, defaultSepWhileCRLF, appendCRLF);
        } else {
            err(arr, defaultSepWhileNotCrlf, appendCRLF);
        }
    }

    @Override
    public <T> void err(T[] arr) {
        err(arr, errputAppendCrlfForContainer);
    }

    @Override
    public <T> void err(T[] arr, String sep) {
        err(arr, sep, errputAppendCrlfForContainer);
    }

    @Override
    public void err(boolean[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(boolean[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void err(byte[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(byte[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void err(char[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(char[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void err(int[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(int[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void err(long[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(long[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void err(float[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(float[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void err(double[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(double[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public <T> void err(T[][] arr, String sep) {
        log(arr, sep, Constants.ERR_IDX);
    }

    @Override
    public <T> void err(T[][] arr) {
        err(arr, defaultSepWhileTwoDimen);
    }

    @Override
    public void err(boolean[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(boolean[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void err(byte[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(byte[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void err(char[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(char[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void err(int[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(int[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void err(long[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(long[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void err(float[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(float[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void err(double[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public void err(double[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public <T> void err(T[] arr, Iterator<Integer> it, String sep) {
        log(arr, it, sep, Constants.ERR_IDX);
    }

    @Override
    public <T> void err(T[] arr, Iterator<Integer> it) {
        err(arr, it, defaultSepWhileNotCrlf);
    }

    @Override
    public void err(String logPattern, JSONObject argsMap) {
        log(logPattern, argsMap, Constants.ERR_IDX);
    }

    @Override
    public <T> void err(String logPattern, Object... args) {
        log(logPattern, args, Constants.ERR_IDX);
    }

    @Override
    public <T> void errWithIdx(String logPattern, Object... args) {
        logWithIdx(logPattern, args, Constants.ERR_IDX);
    }

    @Override
    public void err(boolean bool01, boolean bool02) {
        err(String.valueOf(bool01) + defaultSepWhileNotCrlf + String.valueOf(bool02));
    }

    @Override
    public void err(byte row, byte col) {
        err(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void err(char row, char col) {
        err(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void err(int row, int col) {
        err(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void err(long row, long col) {
        err(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void err(float row, float col) {
        err(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public void err(double row, double col) {
        err(row + defaultSepWhileNotCrlf + col);
    }

    @Override
    public <T1, T2> void err(T1 row, T2 col) {
        err(row.toString() + defaultSepWhileNotCrlf + col.toString());
    }

    @Override
    public void errHorizon(int n) {
        logHorizon(n, Constants.ERR_IDX);
    }

    @Override
    public void errHorizon() {
        errHorizon(1);
    }

    @Override
    public void errEnter() {
        errEnter(1);
    }

    @Override
    public void errEnter(int n) {
        logEnter(n, Constants.ERR_IDX);
    }

    @Override
    public void flush() {
        try {
            Set<String> flushed = new HashSet<>();
            for (int i = 0; i < LoggerConstants.LOG_FILES.length; i++) {
                if (outToLogFile[i] && (!flushed.contains(logBuffNames[i]))) {
                    Tools.flushBuffer(logBuffNames[i]);
                    flushed.add(logBuffNames[i]);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public long size(int modeIdx) {
        BuffInfo buffInfo = Tools.getBuffInfo(logBuffNames[modeIdx]);
        if (buffInfo == null) {
            return -1;
        }

        return buffInfo.getSb().length();
    }

    @Override
    public long sizeOfOut() {
        return size(Constants.OUT_IDX);
    }

    @Override
    public long sizeOfErr() {
        return size(Constants.ERR_IDX);
    }


    // ------------ 待续 --------------------


    // ------------ 辅助方法 --------------------


    /**
     * 获取当前Log的buffNames
     *
     * @param logBuffSuffix logBuffSuffix
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/16/2017 7:58 PM
     * @since 1.0
     */
    private String genLogBuffNames(String logBuffSuffix) {
        return BUFF_NAME_PREFIX + BUFF_NAME_SEP + loggerId + BUFF_NAME_SEP + logBuffSuffix;
    }

    /**
     * 配置给定的输出模式对应的输出文件给logFile
     *
     * @param logFile 配置输出的文件
     * @param modeIdx 给定的输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:16 PM
     * @since 1.0
     */
    private void setLogFile0(String logFile, int modeIdx) throws Exception {
        if (logFiles[modeIdx] != null) {
            setLogFile00(logFile, modeIdx);
        } else {
            log(logModes[modeIdx] + "'s outputFile is null, maybe not support out log to 'logFile', use 'setXXXToLogFile' insted !");
        }
    }

    /**
     * 配置给定的输出模式对应的输出文件给logFile
     *
     * @param logFile 配置输出的文件
     * @param modeIdx 给定的输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:16 PM
     * @since 1.0
     */
    private void setLogFile00(String logFile, int modeIdx) throws Exception {
        // 和当前流输出相同的第一个流的索引[校验是否需要关流, 以及更新logBufName]
        int firstNonMeSameBuffIdx = -1;
        for (int i = 0; i < logModes.length; i++) {
            if (i == modeIdx) continue;
            if (outToLogFile[i] && (logFiles[modeIdx].equals(logFiles[i]))) {
                firstNonMeSameBuffIdx = i;
                break;
            }
        }

        // 如果是设置logFile为空, 则校验是否需要关掉缓冲, 如果需要则关掉缓冲
        if (logFile == null) {
            if ((firstNonMeSameBuffIdx < 0) && Tools.bufExists(logBuffNames[modeIdx])) {
                Tools.closeAnBuffer(logBuffNames[modeIdx]);
            }
            outToLogFile[modeIdx] = false;
            logFiles[modeIdx] = null;
            return;
        }

        // 和logFile相同的第一个非当前流的索引[校验是否需要创建新的缓冲]
        int sameBufIdx = -1;
        for (int i = 0; i < logModes.length; i++) {
            if (i == modeIdx) continue;
            if (logFile.equals(logFiles[i])) {
                sameBufIdx = i;
                break;
            }
        }
        boolean needCreateNewBuf = sameBufIdx < 0;
        String oldBufName = logBuffNames[modeIdx];
        // 如果需要创建新的buff, 则更新logBufNames[modeIdx]
        // 如果logBufNames[modeIdx] 和Constants.logBufNames[modeIdx]相同, 表示有其他的流关联在logBufNames[modeIdx]上面, 更新其他的流的
        if (needCreateNewBuf) {
            // 更新可能存在的多个关联在同一个缓冲上面的其他缓冲的key[散]
            if (logBuffNames[modeIdx].equals(genLogBuffNames(LoggerConstants.LOG_BUFF_SIFFIXES[modeIdx]))) {
                if (firstNonMeSameBuffIdx >= 0) {
                    for (int i = firstNonMeSameBuffIdx; i < logModes.length; i++) {
                        if (logBuffNames[i].equals(logBuffNames[modeIdx])) {
                            logBuffNames[i] = logBuffNames[firstNonMeSameBuffIdx];
                        }
                    }
                    // 关闭原来的缓冲[由之后的createBuffer创建], 然后为关联在当前流的其他流创建新的缓冲[暂不考虑并发情况]
                    Tools.closeAnBuffer(logBuffNames[modeIdx]);
                    Tools.createAnBuffer(genLogBuffNames(LoggerConstants.LOG_BUFF_SIFFIXES[firstNonMeSameBuffIdx]), logBuffNames[firstNonMeSameBuffIdx]);
                }
            }
            logBuffNames[modeIdx] = genLogBuffNames(LoggerConstants.LOG_BUFF_SIFFIXES[modeIdx]);
            // 更新当前缓冲为已经存在的缓冲[聚]
        } else {
            logBuffNames[modeIdx] = logBuffNames[sameBufIdx];
        }

        // 如果logFiles[modeIdx]不为空, 并且logFile 与logFiles[modeIdx]不相同, 则根据情况, 创建缓冲
        // 否则, 创建缓冲, 或者doNothing
        if ((logFiles[modeIdx] != null) && (!logFiles[modeIdx].equals(logFile))) {
//			if((firstNonMeSameBuffIdx < 0) && Tools.bufExists(logBufNames[modeIdx]) ) {
            if ((firstNonMeSameBuffIdx < 0) && Tools.bufExists(oldBufName)) {
                Tools.flushBuffer(oldBufName, true);
            }
            if (needCreateNewBuf) {
                Tools.createAnBuffer(logBuffNames[modeIdx], logFile);
            }
        } else {
            if (!Tools.bufExists(logBuffNames[modeIdx])) {
                Tools.createAnBuffer(logBuffNames[modeIdx], logFile);
            } else {
                log("specified : 'logFile' is currentStartIdx 'Log.logFile', ignore !");
            }
        }
        logFiles[modeIdx] = logFile;
    }

    /**
     * 配置给定的输出模式 是否需要输出到文件, 以及对应的输出文件给logFile
     *
     * @param toLogFile 是否需要输出到文件
     * @param logFile   配置输出的文件
     * @param modeIdx   给定的输出模式的索引
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:17 PM
     * @since 1.0
     */
    private void setToLogFile0(boolean toLogFile, String logFile, int modeIdx) throws Exception {
        outToLogFile[modeIdx] = toLogFile;
        if (toLogFile) {
            setLogFile0(logFile, modeIdx);
        }
    }


}
