/**
 * file name : LogPatternUtils.java
 * created at : 下午9:38:27 2016年9月23日
 * created by 970655147
 */

package com.hx.log.util;

import java.util.AbstractMap;

import com.hx.attrHandler.attrHandler.operation.interf.OperationAttrHandler;
import com.hx.attrHandler.util.AttrHandlerUtils;
import com.hx.log.util.LogPattern.ConstantsLogPattern;
import com.hx.log.util.LogPattern.DateLogPattern;
import com.hx.log.util.LogPattern.HandlerLogPattern;
import com.hx.log.util.LogPattern.IncIndexLogPattern;
import com.hx.log.util.LogPattern.LogPatternChain;
import com.hx.log.util.LogPattern.OptionalLogPattern;
import com.hx.log.util.LogPattern.StackTraceLogPattern;
import com.hx.log.util.LogPattern.ThreadLogPattern;
import com.hx.log.util.LogPattern.VarLogPattern;

import net.sf.json.JSONObject;

public final class LogPatternUtils {

	// disable constructor
	private LogPatternUtils() {
		Tools.assert0("can't instantiate !");
	}
	
	// ----------------------------------- 相关业务方法 ------------------------------------------
	// ------------ 格式化日期相关 ------- 2016.04.21 -------------
	// 根据给定的logPattern获取打印日志所需的LogPatternChain
	public static LogPatternChain initLogPattern(String logPattern) {
		if(logPattern == null) {
			return Constants.JUST_PRINT_MSG_LOG_PATTERN;
		}
		
		WordsSeprator sep = new WordsSeprator(logPattern, Constants.logPatternSeps, null, true);
		return initLogPattern(sep);
	}
	public static LogPatternChain initLogPattern(WordsSeprator sep) {
		if(sep == null) {
			return Constants.JUST_PRINT_MSG_LOG_PATTERN;
		}
		
		LogPatternChain logPatternChain = new LogPatternChain();
		while(sep.hasNext() ) {
			String next = sep.next();
			switch (next) {
				case Constants.VAR_START:
				{
					assert0(sep.hasNext(), "unExpected end of 'logPattern'! ");
					String varName = sep.next().trim();
					switch (varName) {
						case Constants.LOG_PATTERN_DATE:
							logPatternChain.addLogPattern(new DateLogPattern(Constants.DATE_FORMAT) );
							break;
//						case Constants.LOG_PATTERN_MODE:
//							logPatternChain.addLogPattern(new ModeLogPattern(Constants.LOG_MODES[Constants.OUT_IDX]) );	
//							break;
//						case Constants.LOG_PATTERN_MSG:
//							logPatternChain.addLogPattern(new MsgLogPattern(Constants.DEFAULT_VAR_VALUE) );	
//							break;
//						case Constants.LOG_PATTERN_LOG_IDX:
//							logPatternChain.addLogPattern(new LogIdxLogPattern(Constants.DEFAULT_VAR_VALUE) );	
//							break;
						case Constants.LOG_PATTERN_IDX:
							// ${idx }
							if(! Constants.LBRACKET.equals(sep.seek()) ) {
								logPatternChain.addLogPattern(new IncIndexLogPattern(0, 1) );
								break ;
							}
							// ${idx() }
							sep.next();
							if(Constants.RBRACKET.equals(sep.seek()) ) {
								logPatternChain.addLogPattern(new IncIndexLogPattern(0, 1) );
								sep.next();
								break ;
							}
							// ${idx(2) } or $idx(2, 4)
							String initValOrAndInc = sep.next();
							int commaIdx = initValOrAndInc.indexOf(",");
							int inc = 1;
							int initVal = 0;
							// 'Integer.parseInt' may got 'NumberFormatException'
							if(commaIdx >= 0) {
								inc = Integer.parseInt(initValOrAndInc.substring(commaIdx + 1).trim() );
								initVal = Integer.parseInt(initValOrAndInc.substring(0, commaIdx).trim() );
							} else {
								initVal = Integer.parseInt(initValOrAndInc.trim() );
							}
							logPatternChain.addLogPattern(new IncIndexLogPattern(initVal, inc) );
							assert0(Constants.RBRACKET.equals(sep.next()), "expect a ')', but got an : '" + sep.seekLastNext() + "' !" );
							break;
						case Constants.LOG_PATTERN_HANDLER :
							// add for compatiable with '${handler }' at 2016.09.24
							if(Constants.VAR_END.equals(sep.seek()) ) {
								logPatternChain.addLogPattern(new VarLogPattern(varName) );
								break ;
							}
							assert0(Constants.LBRACKET.equals(sep.next()), "expect a '(', but go an : '" + sep.seekLastNext() + "' !");
							int stackCnt = 1;
							StringBuilder sb = new StringBuilder(sep.length() - sep.lastNextPos() );
							while(sep.hasNext() ) {
								String partHandlerStr = sep.next();
								if(Constants.LBRACKET.equals(partHandlerStr) ) {
									stackCnt ++;
								}
								if(Constants.RBRACKET.equals(partHandlerStr) ) {
									stackCnt --;
								}
								if(stackCnt == 0) {
									break ;
								}
								sb.append(partHandlerStr);
							}
							assert0(Constants.RBRACKET.equals(sep.seekLastNext()), "expect 'handler()' endsWith ')', but got an : '" + sep.seekLastNext() + "' !");
							String handlerStr = sb.toString();
							OperationAttrHandler operationHandler = AttrHandlerUtils.handlerParse(handlerStr, Constants.HANDLER);
							logPatternChain.addLogPattern(new HandlerLogPattern(operationHandler, Constants.DEFAULT_VAR_VALUE) );
							break ;
						case Constants.LOG_PATTERN_THREAD:
							logPatternChain.addLogPattern(new ThreadLogPattern() );
							break;
						case Constants.LOG_PATTERN_STACK_TRACE:
							logPatternChain.addLogPattern(new StackTraceLogPattern() );
							break;
//						case Constants.LOG_PATTERN_TASK_NAME:
//							logPatternChain.addLogPattern(new TaskNameLogPattern(Constants.DEFAULT_VAR_VALUE) );	
//							break;
//						case Constants.LOG_PATTERN_URL:
//							logPatternChain.addLogPattern(new UrlLogPattern(Constants.DEFAULT_VAR_VALUE) );	
//							break;
//						case Constants.LOG_PATTERN_RESULT:
//							logPatternChain.addLogPattern(new ResultLogPattern(Constants.DEFAULT_VAR_VALUE) );	
//							break;
//						case Constants.LOG_PATTERN_SPENT:
//							logPatternChain.addLogPattern(new SpentLogPattern(Constants.DEFAULT_VAR_VALUE) );	
//							break;
//						case Constants.LOG_PATTERN_EXCEPTION:
//							logPatternChain.addLogPattern(new ExceptionLogPattern(Constants.DEFAULT_VAR_VALUE) );	
//							break;										
						default:
							// replace 'ConstantsLogPattern' -> 'VarLogPattern' at 2016.09.23
//							String constantsValue = (props == null) ? DEFAULT_VAR_VALUE : (props.get(varName) != null) ? props.get(varName) : DEFAULT_VAR_VALUE;
//							logPatternChain.addLogPattern(new ConstantsLogPattern(constantsValue) );
							logPatternChain.addLogPattern(new VarLogPattern(varName) );
							break;
					}
					assert0(Constants.VAR_END.equals(sep.next() ), "expect an '" + Constants.VAR_END + "', but got an '" + sep.seekLastNext() + "' ! ");
					break;
				}
				case Constants.OPT_START:
				{
					int stackCnt = 1;
					StringBuilder sb = new StringBuilder(sep.length() - sep.lastNextPos() );
					while(sep.hasNext() ) {
						String partHandlerStr = sep.next();
						if(Constants.OPT_START.equals(partHandlerStr) ) {
							stackCnt ++;
						}
						if(Constants.OPT_END.equals(partHandlerStr) ) {
							stackCnt --;
						}
						if(stackCnt == 0) {
							break ;
						}
						sb.append(partHandlerStr);
					}
					assert0(Constants.OPT_END.equals(sep.seekLastNext()), "expect '$[' endsWith ']', but got an : '" + sep.seekLastNext() + "' !");
					String optionalStr = sb.toString();
					
					LogPatternChain chain = initLogPattern(optionalStr);
					logPatternChain.addLogPattern(new OptionalLogPattern(chain, Constants.DEFAULT_VAR_VALUE) );
					break ;
				}
				default:
				{
					logPatternChain.addLogPattern(new ConstantsLogPattern(next) );
					break;
				}
			}
		}
		
		return logPatternChain;
	}
	// incase of 'initDependency'[Tools.taskBeforeLogPatternChain == null]		add at 2016.05.19
	private static void assert0(boolean boo, String msg) {
		if(msg == null) {
			System.err.println("'msg' can't be null ");
			return ;
		}
		if(! boo) {
			throw new RuntimeException("assert0Exception : " + msg);
		}
	}

	// 格式化日期相关
	public static String formatLogInfo(LogPatternChain logPatternChain, JSONObject argsMap) {
		if(logPatternChain == null) {
			return argsMap.optString(Constants.LOG_PATTERN_MSG );
		}
		
		logPatternChain.setResult(null );
		for(LogPattern logPattern : logPatternChain.getChain() ) {
			switch (logPattern.type() ) {
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
				case VAR :
				{
					VarLogPattern varLogPattern = (VarLogPattern) logPattern;
					String val = Tools.optString(argsMap, varLogPattern.key, Constants.DEFAULT_VAR_VALUE);
					varLogPattern.setArg(val);
					break ;
				}
				case HANDLER :
				{
					HandlerLogPattern handlerLogPattern = (HandlerLogPattern) logPattern;
					String val = Tools.optString(argsMap, Constants.LOG_PATTERN_HANDLER, Constants.DEFAULT_VAR_VALUE);
					handlerLogPattern.setArg(val);
					break ;
				}
				case PATTERN_CHAIN :
				{
					LogPatternChain logPatternChainCur = (LogPatternChain) logPattern;
					String formatedLogInfo = formatLogInfo(logPatternChainCur, argsMap);
					logPatternChainCur.setResult(formatedLogInfo);
					break ;
				}
				case OPTIONAL :
				{
					OptionalLogPattern optionalLogPattern = (OptionalLogPattern) logPattern;
					formatOptionalInfo(optionalLogPattern, argsMap);
					break ;
				}
				default:
				{
					break;
				}
			}
		}
		
		return logPatternChain.pattern();
	}
	public static String formatLogInfo(LogPatternChain logPatternChain, AbstractMap<String, String> argsMap) {
		return formatLogInfo(logPatternChain, JSONObject.fromObject(argsMap) );
	}
	private static void formatOptionalInfo(OptionalLogPattern optionalLogPattern, JSONObject argsMap) {
		outerLoop :
		for(LogPattern logPattern : optionalLogPattern.chain.getChain() ) {
			switch (logPattern.type() ) {
				case VAR :
				{
					VarLogPattern varLogPattern = (VarLogPattern) logPattern;
					String val = Tools.optString(argsMap, varLogPattern.key, Constants.EMPTY_STR);
					if(! Tools.isEmpty(val) ) {
						varLogPattern.setArg(val);
					} else {
						optionalLogPattern.setResult(Constants.EMPTY_STR);
						break outerLoop;
					}
					break ;
				}
				case HANDLER :
				{
					HandlerLogPattern handlerLogPattern = (HandlerLogPattern) logPattern;
					String val = Tools.optString(argsMap, Constants.LOG_PATTERN_HANDLER, Constants.DEFAULT_VAR_VALUE);
					handlerLogPattern.setArg(val);
					break ;
				}				
				case PATTERN_CHAIN :
				{
					LogPatternChain logPatternChainCur = (LogPatternChain) logPattern;
					String formatedLogInfo = formatLogInfo(logPatternChainCur, argsMap);
					logPatternChainCur.setResult(formatedLogInfo);
					break ;
				}
				case OPTIONAL :
				{
					OptionalLogPattern optionalLogPatternTmp = (OptionalLogPattern) logPattern;
					formatOptionalInfo(optionalLogPatternTmp, argsMap);
					break ;
				}
				default:
				{
					break;
				}							
			}
		}
	}
	
}
