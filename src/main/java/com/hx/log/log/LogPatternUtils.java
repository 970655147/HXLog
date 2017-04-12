/**
 * file name : LogPatternUtils.java
 * created at : 下午9:38:27 2016年9月23日
 * created by 970655147
 */

package com.hx.log.log;

import com.hx.attr_handler.attr_handler.operation.interf.OperationAttrHandler;
import com.hx.attr_handler.util.AttrHandlerUtils;
import com.hx.log.interf.LogPattern;
import com.hx.log.log.log_pattern.*;
import com.hx.log.str.WordsSeprator;
import com.hx.log.util.Constants;
import com.hx.log.util.Tools;
import net.sf.json.JSONObject;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public final class LogPatternUtils {

    // disable constructor
    private LogPatternUtils() {
        Tools.assert0("can't instantiate !");
    }

    // ----------------------------------- 相关业务方法 ------------------------------------------
    // ------------ 格式化日期相关 ------- 2016.04.21 -------------
    // 根据给定的logPattern获取打印日志所需的LogPatternChain
    public static LogPatternChain initLogPattern(String logPattern) {
        if (logPattern == null) {
            return Constants.JUST_PRINT_MSG_LOG_PATTERN;
        }

        WordsSeprator sep = new WordsSeprator(logPattern, Constants.logPatternSeps, null, true);
        return initLogPattern(sep);
    }

    public static LogPatternChain initLogPattern(WordsSeprator sep) {
        if (sep == null) {
            return Constants.JUST_PRINT_MSG_LOG_PATTERN;
        }

        LogPatternChain logPatternChain = new LogPatternChain();
        while (sep.hasNext()) {
            String next = sep.next();
            switch (next) {
                case Constants.VAR_START: {
                    assert0(sep.hasNext(), "unExpected end of 'logPattern'! ");
                    String varName = sep.next().trim();
                    switch (varName) {
                        case Constants.LOG_PATTERN_DATE:
                            logPatternChain.addLogPattern(new DateLogPattern(Constants.DATE_FORMAT));
                            break;
                        case Constants.LOG_PATTERN_IDX:
                            // ${idx }
                            if (!Constants.LBRACKET.equals(sep.seek())) {
                                logPatternChain.addLogPattern(new IncIndexLogPattern(0, 1));
                                break;
                            }
                            // ${idx() }
                            sep.next();
                            if (Constants.RBRACKET.equals(sep.seek())) {
                                logPatternChain.addLogPattern(new IncIndexLogPattern(0, 1));
                                sep.next();
                                break;
                            }
                            // ${idx(2) } or $idx(2, 4)
                            String initValOrAndInc = sep.next();
                            int commaIdx = initValOrAndInc.indexOf(",");
                            int inc = 1;
                            int initVal = 0;
                            // 'Integer.parseInt' may got 'NumberFormatException'
                            if (commaIdx >= 0) {
                                inc = Integer.parseInt(initValOrAndInc.substring(commaIdx + 1).trim());
                                initVal = Integer.parseInt(initValOrAndInc.substring(0, commaIdx).trim());
                            } else {
                                initVal = Integer.parseInt(initValOrAndInc.trim());
                            }
                            logPatternChain.addLogPattern(new IncIndexLogPattern(initVal, inc));
                            assert0(Constants.RBRACKET.equals(sep.next()), "expect a ')', but got an : '" + sep.seekLastNext() + "' !");
                            break;
                        case Constants.LOG_PATTERN_HANDLER:
                            // add for compatiable with '${handler }' at 2016.09.24
                            if (Constants.VAR_END.equals(sep.seek())) {
                                logPatternChain.addLogPattern(new VarLogPattern(varName));
                                break;
                            }
                            assert0(Constants.LBRACKET.equals(sep.next()), "expect a '(', but go an : '" + sep.seekLastNext() + "' !");
                            int stackCnt = 1;
                            StringBuilder sb = new StringBuilder(sep.length() - sep.lastNextPos());
                            while (sep.hasNext()) {
                                String partHandlerStr = sep.next();
                                if (Constants.LBRACKET.equals(partHandlerStr)) {
                                    stackCnt++;
                                }
                                if (Constants.RBRACKET.equals(partHandlerStr)) {
                                    stackCnt--;
                                }
                                if (stackCnt == 0) {
                                    break;
                                }
                                sb.append(partHandlerStr);
                            }
                            assert0(Constants.RBRACKET.equals(sep.seekLastNext()), "expect 'handler()' endsWith ')', but got an : '" + sep.seekLastNext() + "' !");
                            String handlerStr = sb.toString();
                            OperationAttrHandler operationHandler = AttrHandlerUtils.handlerParse(handlerStr, Constants.HANDLER);
                            logPatternChain.addLogPattern(new HandlerLogPattern(operationHandler, Constants.DEFAULT_VAR_VALUE));
                            break;
                        case Constants.LOG_PATTERN_THREAD:
                            logPatternChain.addLogPattern(new ThreadLogPattern());
                            break;
                        case Constants.LOG_PATTERN_STACK_TRACE:
                            logPatternChain.addLogPattern(new StackTraceLogPattern());
                            break;
                        case Constants.LOG_PATTERN_LINE_INFO:
                            logPatternChain.addLogPattern(new LineInfoLogPattern());
                            break;
                        default:
                            // replace 'ConstantsLogPattern' -> 'VarLogPattern' at 2016.09.23
//							String constantsValue = (props == null) ? DEFAULT_VAR_VALUE : (props.get(varName) != null) ? props.get(varName) : DEFAULT_VAR_VALUE;
//							logPatternChain.addLogPattern(new ConstantsLogPattern(constantsValue) );
                            logPatternChain.addLogPattern(new VarLogPattern(varName));
                            break;
                    }
                    assert0(Constants.VAR_END.equals(sep.next()), "expect an '" + Constants.VAR_END + "', but got an '" + sep.seekLastNext() + "' ! ");
                    break;
                }
                case Constants.OPT_START: {
                    int stackCnt = 1;
                    StringBuilder sb = new StringBuilder(sep.length() - sep.lastNextPos());
                    while (sep.hasNext()) {
                        String partHandlerStr = sep.next();
                        if (Constants.OPT_START.equals(partHandlerStr)) {
                            stackCnt++;
                        }
                        if (Constants.OPT_END.equals(partHandlerStr)) {
                            stackCnt--;
                        }
                        if (stackCnt == 0) {
                            break;
                        }
                        sb.append(partHandlerStr);
                    }
                    assert0(Constants.OPT_END.equals(sep.seekLastNext()), "expect '$[' endsWith ']', but got an : '" + sep.seekLastNext() + "' !");
                    String optionalStr = sb.toString();

                    LogPatternChain chain = initLogPattern(optionalStr);
                    logPatternChain.addLogPattern(new OptionalLogPattern(chain, Constants.DEFAULT_VAR_VALUE));
                    break;
                }
                default: {
                    logPatternChain.addLogPattern(new ConstantsLogPattern(next));
                    break;
                }
            }
        }

        return logPatternChain;
    }

    // incase of 'initDependency'[Tools.taskBeforeLogPatternChain == null]		add at 2016.05.19
    private static void assert0(boolean boo, String msg) {
        if (msg == null) {
            System.err.println("'msg' can't be null ");
            return;
        }
        if (!boo) {
            throw new RuntimeException("assert0Exception : " + msg);
        }
    }

    // 格式化日期相关
    public static String formatLogInfo(LogPatternChain logPatternChain, JSONObject argsMap) {
        if (logPatternChain == null) {
            return argsMap.optString(Constants.LOG_PATTERN_MSG);
        }

        logPatternChain.setResult(null);
        for (LogPattern logPattern : logPatternChain.getChain()) {
            switch (logPattern.type()) {
                // use 'Mode' instedof 'LogPatternType.Mode'
                // from : http://caohongxing7604.blog.163.com/blog/static/32016974200991412040387/
//				case MODE:
//				case MSG:
//				case LOG_IDX:
//				case HANDLER :
//				case TASK_NAME :
//				case URL :
//				case RESULT :
//				case SPENT :
//				case EXCEPTION :
//					((OneStringVariableLogPattern) logPattern).setArg(argsMap.optString(logPattern.type().typeKey(), Constants.DEFAULT_VAR_VALUE) );
//					break ;
                case VAR: {
                    VarLogPattern varLogPattern = (VarLogPattern) logPattern;
                    String val = Tools.optString(argsMap, varLogPattern.key, Constants.DEFAULT_VAR_VALUE);
                    varLogPattern.setArg(val);
                    break;
                }
                case HANDLER: {
                    HandlerLogPattern handlerLogPattern = (HandlerLogPattern) logPattern;
                    String val = Tools.optString(argsMap, Constants.LOG_PATTERN_HANDLER, Constants.DEFAULT_VAR_VALUE);
                    handlerLogPattern.setArg(val);
                    break;
                }
                case PATTERN_CHAIN: {
                    LogPatternChain logPatternChainCur = (LogPatternChain) logPattern;
                    String formatedLogInfo = formatLogInfo(logPatternChainCur, argsMap);
                    logPatternChainCur.setResult(formatedLogInfo);
                    break;
                }
                case OPTIONAL: {
                    OptionalLogPattern optionalLogPattern = (OptionalLogPattern) logPattern;
                    formatOptionalInfo(optionalLogPattern, argsMap);
                    break;
                }
                default: {
                    break;
                }
            }
        }

        return logPatternChain.pattern();
    }

    public static String formatLogInfo(LogPatternChain logPatternChain, AbstractMap<String, String> argsMap) {
        return formatLogInfo(logPatternChain, JSONObject.fromObject(argsMap));
    }

    private static void formatOptionalInfo(OptionalLogPattern optionalLogPattern, JSONObject argsMap) {
        outerLoop:
        for (LogPattern logPattern : optionalLogPattern.chain.getChain()) {
            switch (logPattern.type()) {
                case VAR: {
                    VarLogPattern varLogPattern = (VarLogPattern) logPattern;
                    String val = Tools.optString(argsMap, varLogPattern.key, Constants.EMPTY_STR);
                    if (!Tools.isEmpty(val)) {
                        varLogPattern.setArg(val);
                    } else {
                        optionalLogPattern.setResult(Constants.EMPTY_STR);
                        break outerLoop;
                    }
                    break;
                }
                case HANDLER: {
                    HandlerLogPattern handlerLogPattern = (HandlerLogPattern) logPattern;
                    String val = Tools.optString(argsMap, Constants.LOG_PATTERN_HANDLER, Constants.DEFAULT_VAR_VALUE);
                    handlerLogPattern.setArg(val);
                    break;
                }
                case PATTERN_CHAIN: {
                    LogPatternChain logPatternChainCur = (LogPatternChain) logPattern;
                    String formatedLogInfo = formatLogInfo(logPatternChainCur, argsMap);
                    logPatternChainCur.setResult(formatedLogInfo);
                    break;
                }
                case OPTIONAL: {
                    // the inner OptionalLogPattern doesn't influence outer OptionalLogpattern
                    OptionalLogPattern optionalLogPatternTmp = (OptionalLogPattern) logPattern;
                    formatOptionalInfo(optionalLogPatternTmp, argsMap);
                    break;
                }
                default: {
                    break;
                }
            }
        }
    }


    /**
     * @param logPattern 给定的pattern
     * @param marks      分隔符的字符串
     * @param args       参数
     * @return
     * @Name: formatLogInfo
     * @Description: 格式化给定的logPattern[Constants.VAR_PLACE为占位符, 依次将各个占位符替换为args的各个值]
     * @Create at 2016-11-23 22:00:18 by '970655147'
     */
    public static String formatLogInfo(String logPattern, Set<String> marks, Object... args) {
        Tools.assert0(logPattern != null, "'logPattern' can't be null !");
        Tools.assert0(marks != null, "'remarks' can't be null !");

        WordsSeprator sep = new WordsSeprator(logPattern, marks, null, true, true);
        StringBuilder sb = new StringBuilder(logPattern.length() << 1);
        int idx = 0;
        while (sep.hasNext()) {
            sb.append(sep.next());
            // skip "{}"
            if (sep.hasNext()) {
                String mark = sep.next();
                if ((args == null) || (idx >= args.length)) {
                    sb.append(mark);
                } else {
                    sb.append(String.valueOf(args[idx++]));
                }
            }
        }

        return sb.toString();
    }

    public static String formatLogInfo(String logPattern, Object... args) {
        return formatLogInfo(logPattern, Tools.asSet(Constants.VAR_PLACE), args);
    }


    /**
     * 根据给定的logPattern格式化信息
     * val pat = 'this is first param {0}, and this is second param {1} .'
     * formatLogInfo(pat, '{', '}', 'param01', 'param02' )
     * you'll got : "this is first param param01, and this is second param param02 ."
     *
     * @param logPattern   给定的pattern
     * @param leftAndRight 括号对
     * @param args         参数
     * @return
     * @Create at 2016-12-05 11:39:00 by '970655147'
     */
    public static String formatLogInfoWithIdx(String logPattern, Map<String, String> leftAndRight, Object... args) {
        Tools.assert0(logPattern != null, "\'logPattern\' can't be null !");
        Tools.assert0(leftAndRight != null, "\'leftAndRight\' can't be null !");
        Tools.assert0(!leftAndRight.containsKey(null), "\'leftAndRight\' can't contains null !");

        Set<String> leftAndRightMap = Tools.<String>asLinkedSet();
        for (Entry<String, String> entry : leftAndRight.entrySet()) {
            if ((entry.getValue() == null)) {
                Tools.assert0("\'leftAndRight\' can't contains null !");
            }
            leftAndRightMap.add(entry.getKey());
            leftAndRightMap.add(entry.getValue());
        }

        WordsSeprator sep = new WordsSeprator(logPattern, leftAndRightMap, null, true, true);
        StringBuilder sb = new StringBuilder(logPattern.length() << 1);
        while (sep.hasNext()) {
            String next = sep.next();
            if (leftAndRight.containsKey(next)) {
                String idxStr = sep.next();
                String expectRight = sep.next();
                int idx = 0;
                try {
                    idx = Integer.parseInt(idxStr);
                } catch (Exception e) {
                    idx = -1;
                }

                if (leftAndRight.get(next).equals(expectRight) && ((args != null) && (idx >= 0) && (idx < args.length))) {
                    sb.append(args[idx]);
                } else {
                    sb.append(next);
                    if (idxStr != null) {
                        sb.append(idxStr);
                        if (expectRight != null) {
                            sb.append(expectRight);
                        }
                    }
                }
            } else {
                sb.append(next);
            }
        }

        return sb.toString();
    }

    public static String formatLogInfoWithIdx(String logPattern, Object... args) {
        return formatLogInfoWithIdx(logPattern, Tools.asLinkedMap("{", "}"), args);
    }


}
