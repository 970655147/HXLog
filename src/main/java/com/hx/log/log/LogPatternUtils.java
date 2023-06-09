/**
 * file name : LogPatternUtils.java
 * created at : 下午9:38:27 2016年9月23日
 * created by 970655147
 */

package com.hx.log.log;

import com.hx.attr_handler.attr_handler.operation.interf.OperationAttrHandler;
import com.hx.attr_handler.util.AttrHandlerUtils;
import com.hx.common.str.WordsSeprator;
import com.hx.common.util.InnerTools;
import com.hx.json.JSONObject;
import com.hx.log.log.log_pattern.*;
import com.hx.log.log.log_pattern.interf.LogPattern;
import com.hx.log.util.Constants;
import com.hx.log.util.Tools;

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

    /**
     * 根据给定的logPattern表达式获取打印日志所需的LogPatternChain
     *
     * @param sep logPattern表达式根据指定分隔符拆分后的spliter
     * @return com.hx.log.log.log_pattern.LogPatternChain
     * @author Jerry.X.He
     * @date 5/1/2017 11:43 AM
     * @since 1.0
     */
    public static LogPatternChain initLogPattern(WordsSeprator sep) {
        if (sep == null) {
            return Constants.JUST_PRINT_MSG_LOG_PATTERN;
        }

        LogPatternChain logPatternChain = new LogPatternChain();
        while (sep.hasNext()) {
            String next = sep.next();
            switch (next) {
                case LogPatternConstants.VAR_START: {
                    InnerTools.assert0(sep.hasNext(), "unExpected end of 'logPattern'! ");
                    String varName = sep.next().trim();
                    switch (varName) {
                        case LogPatternConstants.LOG_PATTERN_DATE:
                            logPatternChain.addLogPattern(new DateLogPattern(Constants.DATE_FORMAT));
                            break;
                        case LogPatternConstants.LOG_PATTERN_IDX:
                            // ${idx }
                            if (!LogPatternConstants.LBRACKET.equals(sep.seek())) {
                                logPatternChain.addLogPattern(new IncIndexLogPattern(0, 1));
                                break;
                            }
                            // ${idx() }
                            sep.next();
                            if (LogPatternConstants.RBRACKET.equals(sep.seek())) {
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
                            InnerTools.assert0(LogPatternConstants.RBRACKET.equals(sep.next()), "expect a ')', but got an : '" + sep.current() + "' !");
                            break;
                        case LogPatternConstants.LOG_PATTERN_HANDLER:
                            // add for compatiable with '${handler }' at 2016.09.24
                            if (LogPatternConstants.VAR_END.equals(sep.seek())) {
                                logPatternChain.addLogPattern(new VarLogPattern(varName));
                                break;
                            }
                            InnerTools.assert0(LogPatternConstants.LBRACKET.equals(sep.next()), "expect a '(', but go an : '" + sep.current() + "' !");
                            int stackCnt = 1;
                            StringBuilder sb = new StringBuilder(sep.length() - sep.currentStartIdx());
                            while (sep.hasNext()) {
                                String partHandlerStr = sep.next();
                                if (LogPatternConstants.LBRACKET.equals(partHandlerStr)) {
                                    stackCnt++;
                                }
                                if (LogPatternConstants.RBRACKET.equals(partHandlerStr)) {
                                    stackCnt--;
                                }
                                if (stackCnt == 0) {
                                    break;
                                }
                                sb.append(partHandlerStr);
                            }
                            InnerTools.assert0(LogPatternConstants.RBRACKET.equals(sep.current()), "expect 'handler()' endsWith ')', but got an : '" + sep.current() + "' !");
                            String handlerStr = sb.toString();
                            OperationAttrHandler operationHandler = AttrHandlerUtils.handlerParse(handlerStr, Constants.HANDLER);
                            logPatternChain.addLogPattern(new HandlerLogPattern(operationHandler, LogPatternConstants.DEFAULT_VAR_VALUE));
                            break;
                        case LogPatternConstants.LOG_PATTERN_THREAD:
                            logPatternChain.addLogPattern(new ThreadLogPattern());
                            break;
                        case LogPatternConstants.LOG_PATTERN_STACK_TRACE:
                            logPatternChain.addLogPattern(new StackTraceLogPattern());
                            break;
                        case LogPatternConstants.LOG_PATTERN_LINE_INFO:
                            logPatternChain.addLogPattern(new LineInfoLogPattern());
                            break;
                        default:
                            // replace 'ConstantsLogPattern' -> 'VarLogPattern' at 2016.09.23
//							String constantsValue = (props == null) ? DEFAULT_VAR_VALUE : (props.get(varName) != null) ? props.get(varName) : DEFAULT_VAR_VALUE;
//							logPatternChain.addLogPattern(new ConstantsLogPattern(constantsValue) );
                            logPatternChain.addLogPattern(new VarLogPattern(varName));
                            break;
                    }
                    InnerTools.assert0(LogPatternConstants.VAR_END.equals(sep.next()), "expect an '" + LogPatternConstants.VAR_END + "', but got an '" + sep.current() + "' ! ");
                    break;
                }
                case LogPatternConstants.OPT_START: {
                    int stackCnt = 1;
                    StringBuilder sb = new StringBuilder(sep.length() - sep.currentStartIdx());
                    while (sep.hasNext()) {
                        String partHandlerStr = sep.next();
                        if (LogPatternConstants.OPT_START.equals(partHandlerStr)) {
                            stackCnt++;
                        }
                        if (LogPatternConstants.OPT_END.equals(partHandlerStr)) {
                            stackCnt--;
                        }
                        if (stackCnt == 0) {
                            break;
                        }
                        sb.append(partHandlerStr);
                    }
                    InnerTools.assert0(LogPatternConstants.OPT_END.equals(sep.current()), "expect '$[' endsWith ']', but got an : '" + sep.current() + "' !");
                    String optionalStr = sb.toString();

                    LogPatternChain chain = initLogPattern(optionalStr);
                    logPatternChain.addLogPattern(new OptionalLogPattern(chain, LogPatternConstants.DEFAULT_VAR_VALUE));
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

    public static LogPatternChain initLogPattern(String logPattern) {
        if (logPattern == null) {
            return Constants.JUST_PRINT_MSG_LOG_PATTERN;
        }

        WordsSeprator sep = new WordsSeprator(logPattern, LogPatternConstants.LOG_PATTERN_SEPS, null, true);
        return initLogPattern(sep);
    }

    /**
     * 根据给定的参数列表, 获取当前 logPatternChain 参数化之后的结果
     *
     * @param logPatternChain 给定的logPatternChain
     * @param argsMap         参数列表
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:59 PM
     * @since 1.0
     */
    public static String formatLogInfo(LogPatternChain logPatternChain, JSONObject argsMap) {
        if (logPatternChain == null) {
            return argsMap.optString(LogPatternConstants.LOG_PATTERN_MSG);
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
                    String val = Tools.optString(argsMap, varLogPattern.key, LogPatternConstants.DEFAULT_VAR_VALUE);
                    varLogPattern.setArg(val);
                    break;
                }
                case HANDLER: {
                    HandlerLogPattern handlerLogPattern = (HandlerLogPattern) logPattern;
                    String val = Tools.optString(argsMap, LogPatternConstants.LOG_PATTERN_HANDLER, LogPatternConstants.DEFAULT_VAR_VALUE);
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

    /**
     * 根据给定的参数列表, 获取当前 optionalPatternChain 参数化之后的结果
     * OptionalLogPattern的规则是, 如果有一个变量没有找到, 则当前logPattern的结果为null
     *
     * @param optionalLogPattern 给定的optionalPatternChain
     * @param argsMap            参数列表
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 6:00 PM
     * @since 1.0
     */
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
                    String val = Tools.optString(argsMap, LogPatternConstants.LOG_PATTERN_HANDLER, LogPatternConstants.DEFAULT_VAR_VALUE);
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
     * 格式化给定的logPattern[Constants.VAR_PLACE为占位符, 依次将各个占位符替换为args的各个值]
     *
     * @param logPattern 给定的pattern
     * @param marks      分隔符的字符串
     * @param args       参数列表
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 6:02 PM
     * @since 1.0
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
        return formatLogInfo(logPattern, Tools.asSet(LogPatternConstants.VAR_PLACE), args);
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
